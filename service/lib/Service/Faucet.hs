{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Service.Faucet where

import Codec.Serialise
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString as B
import Data.ByteString.Base16 as B16
import Data.ByteString.Builder
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as LC
import Data.List as L
import Data.Map.Strict as M
import Data.Serialize
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import Data.Text.Encoding as DTE

import Service.AllpayTransaction
import Service.Data
import Service.Data.Allegory
import qualified Service.Data.Allegory as Al
import Service.Env
import Service.Nexa
import Service.ProxyProviderUtxo
import Service.Types
    ( AddressOutputs(..)
    , GetUtxosByAddressResponse(..)
    , NexaRequest(..)
    , ProxyProviderException(..)
    , RelayTxResponse(..)
    )
import qualified Service.Types as ST (AddressOutputs(..))
import System.Logger as LG

import Network.Xoken.Address
import Network.Xoken.Block.Merkle
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Script
import Network.Xoken.Transaction
import Network.Xoken.Util

import Network.HTTP.Client

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Data.List
import Data.Maybe
import Network.Xoken.Constants
import qualified NodeConfig as NC

import Data.Int
import LevelDB

giveCoins :: (HasService env m, MonadIO m) => String -> m Bool
giveCoins addrBase58 = do
    nodeCnf <- getNodeConfig
    let net = NC.bitcoinNetwork nodeCnf
        faucetXPrvKey = fromMaybe (throw InvalidFaucetKeyException) $ xPrvImport net (DT.pack $ NC.faucetKey nodeCnf)
        faucetAddress = xPubAddr $ deriveXPubKey $ faucetXPrvKey
        faucetAddressBase58 = DT.unpack $ fromJust $ addrToString net faucetAddress
        faucetScript = addressToScriptBS faucetAddress
        faucetAmount = NC.faucetAmt nodeCnf
        userAddress = fromMaybe (throw UserAddressException) $ stringToAddr net $ DT.pack addrBase58
        userScript = addressToScriptBS userAddress
    sigInputs <- getInputs (NC.nexaHost nodeCnf) faucetAddressBase58 (faucetAmount + 10000) Nothing
    let inputs = (\si -> TxIn (sigInputOP si) faucetScript 0xFFFFFFFF) <$> sigInputs
        change = (L.foldl (+) 0 $ (fromIntegral . sigInputValue) <$> sigInputs) - (faucetAmount + 10000)
        outputs =
            [TxOut (fromIntegral faucetAmount) userScript] ++
            if change == 0
                then []
                else [TxOut (fromIntegral change) faucetScript]
        unsignedTx = Tx 1 inputs outputs 0
    case signTx net unsignedTx sigInputs $ (L.take (L.length inputs) $ L.repeat $ xPrvKey faucetXPrvKey) of
        Right signedTx -> do
            res <- liftIO $ relayTx (NC.nexaHost nodeCnf) (NC.nexaSessionKey nodeCnf) (Data.Serialize.encode signedTx)
            return $ txBroadcast res
        Left err -> return False

getInputs :: (HasService env m, MonadIO m) => String -> String -> Int -> Maybe String -> m [SigInput]
getInputs nexaAddr addr requiredSats cursor
    --lg <- getLogger
 = do
    net <- (return . NC.bitcoinNetwork) =<< (getNodeConfig)
    sk <- NC.nexaSessionKey <$> getNodeConfig
    let req = utxosByAddressRequest nexaAddr addr 10 cursor
    response <- liftIO $ nexaGetReq req (Just sk)
    case A.decode (responseBody response) :: Maybe GetUtxosByAddressResponse of
        Nothing
            --err lg $ LG.msg $ "Error: Failed to parse Nexa response for request '" <> req <> "'"
         -> do
            throw NexaResponseParseException
        Just resp -> do
            scriptPubKey <-
                case stringToAddr net (DT.pack addr) of
                    Nothing
                        --err lg $
                        --    LG.msg $ show "Error: Failed to decode '" <> addr <> "' as a " <> (show net) <> " address"
                     -> do
                        throw NexaResponseParseException
                    Just a' -> return $ addressToOutput a'
            nodeCfg <- getNodeConfig
            let fundingUtxos =
                    sortOn sigInputValue $
                    (\ao ->
                         SigInput
                             scriptPubKey
                             (fromIntegral $ (ST.value :: AddressOutputs -> Int64) ao)
                             (OutPoint
                                  (fromJust $ hexToTxHash $ DT.pack $ outputTxHash ao)
                                  (fromIntegral $ ST.outputIndex ao))
                             (setForkIdFlag sigHashAll)
                             Nothing) <$>
                    utxos resp
            let pickUtxos li rqSats = f [] li rqSats
                  where
                    f li [] _ = li
                    f li (x:xs) s
                        | (L.foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 li) <= s = f (x : li) xs s
                        | otherwise = li
                selectedUtxos = pickUtxos fundingUtxos requiredSats
                gotSats = L.foldl (\p q -> p + (fromIntegral $ sigInputValue q)) 0 selectedUtxos
            if (L.length selectedUtxos == L.length fundingUtxos) && (gotSats < requiredSats)
                then do
                    moreUtxos <- getInputs nexaAddr addr (requiredSats - gotSats) (nextCursor resp)
                    return $ selectedUtxos ++ moreUtxos
                else return selectedUtxos
