{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module UtxoPool where

import Codec.Serialise
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.UTF8 as BL
import Data.Hashable
import Data.Int
import Data.Map as M
import Data.Maybe
import qualified Data.Text as DT (pack)
import GHC.Generics
import GHC.IO.Handle
import Network.Connection
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.Xoken.Address
import Network.Xoken.Constants
import NodeConfig as NC
import qualified Service.Env as SE
import Service.Env
import Service.Nexa
import Service.Types (AddressOutputs(..), GetUtxosByAddressResponse(..), NexaRequest(..), ProxyProviderException(..))
import System.IO
import System.Process

getPoolFromAddress :: Network -> String -> String -> String -> IO [ProxyProviderUtxo]
getPoolFromAddress net nexaAddr poolAddrString sessionKey = do
    let req = utxosByAddressRequest nexaAddr poolAddrString 1000 Nothing
        poolAddress = fromMaybe (throw PoolAddressException) $ stringToAddr net (DT.pack poolAddrString)
    response <- liftIO $ nexaGetReq req (Just sessionKey)
    case A.decode (responseBody response) :: Maybe GetUtxosByAddressResponse of
        Nothing -> do
            putStrLn $
                "Error: Failed to fetch pool. Nexa responded with status " <> (show $ responseStatus response) <>
                ", response body: '" <>
                (show $ responseBody response) <>
                "'"
            throw NexaResponseParseException
        Just resp -> do
            return $
                (\(AddressOutputs addr txHash txIndex _ _ _ _ _ val) ->
                     Unspent addr txHash txIndex (fromIntegral val) (B.unpack $ addressToScriptBS poolAddress)) <$>
                (utxos resp)

refreshPool :: (HasService env m, MonadIO m) => m ()
refreshPool = do
    poolTVar <- getUtxoPool
    nodeCfg <- getNodeConfig
    pool <- liftIO $ readTVarIO poolTVar
    let currPool = M.toList pool
    nextPool' <-
        liftIO $
        getPoolFromAddress
            (NC.bitcoinNetwork nodeCfg)
            (NC.nexaHost nodeCfg)
            (NC.poolAddress nodeCfg)
            (NC.nexaSessionKey nodeCfg)
    let nextPool = (\utxo -> (SE.txid utxo ++ ":" ++ (show $ SE.outputIndex utxo), utxo)) <$> nextPool'
        newPool = M.fromList currPool
    liftIO $ atomically $ writeTVar poolTVar pool
    return ()
