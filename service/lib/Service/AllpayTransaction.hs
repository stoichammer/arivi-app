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

module Service.AllpayTransaction where

import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types hiding (msgType)
import Codec.Compression.GZip as GZ
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, mapConcurrently, mapConcurrently_, race_)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Word (Word32, Word64, Word8)
import Network.Xoken.Address
import Network.Xoken.Block.Merkle
import Network.Xoken.Crypto.Hash
import Network.Xoken.Transaction
import Network.Xoken.Transaction.Common

import Crypto.Secp256k1

--import qualified Control.Error.Util as Extra
import Control.Exception

--import qualified Control.Exception.Lifted as LE (try)
import Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Loops
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16 (decode, encode)
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as BSS

--import Control.Monad.Extra
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Script

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.UTF8 as BSU
import Data.Char
import Data.Default
import Data.Hashable
import Data.IORef
import Data.Int
import Data.List
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import qualified Data.Serialize as S
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding as E
import LevelDB
import Network.Xoken.Constants
import NodeConfig
import Service.Data
import Service.Env
import Service.ProxyProviderUtxo
import Service.Types
import UtxoPool

-- get address, pptuxo and Merkle proofs for address and pputxo
getAddressAndProxyUtxo ::
       (HasService env m, MonadIO m)
    => Network
    -> String
    -> m (Either String (String, ProxyProviderUtxo, PartialMerkleTree, PartialMerkleTree))
getAddressAndProxyUtxo net name = do
    aMapTvar <- getXPubHashMap
    addressTvar <- getAddressMap
    aMap <- liftIO $ readTVarIO aMapTvar
    case M.lookup name aMap of
        Just XPubInfo {..} -> do
            if (count > fromIntegral index)
                then do
                    let addr = xPubAddr (pubSubKey key (index + 1))
                    let utxoOp = utxoCommitment !! (fromIntegral index + 1)
                    mbpputxo <- getCommittedUtxo utxoOp
                    case mbpputxo of
                        Nothing -> return $ Left "failed to fetch proxy-provider utxo; commitment set incomplete?"
                        Just pputxo -> do
                            liftIO $
                                atomically $
                                writeTVar
                                    aMapTvar
                                    (M.update (\(XPubInfo k c i u) -> Just $ XPubInfo k c (i + 1) u) name aMap)
                            addressMap <- liftIO $ readTVarIO addressTvar
                            case M.lookup (xPubExport net key) addressMap of
                                Just hashes -> do
                                    let addrProof = buildProof' hashes index
                                    let utxoProof = buildProof' (outpointHashes utxoCommitment) index
                                    liftIO $
                                        putValue
                                            (DTE.encodeUtf8 $ DT.pack name)
                                            (encodeXPubInfo net $ XPubInfo key count (index + 1) utxoCommitment)
                                    return $
                                        Right
                                            (DT.unpack $ fromJust $ addrToString net addr, pputxo, addrProof, utxoProof)
                else do
                    return $ Left "maximum address count reached"

getPartiallySignedAllpayTransaction ::
       (HasService env m, MonadIO m)
    => Network
    -> [(OutPoint', Int64)] -- standard inputs, corresponding input value
    -> Int64 -- value
    -> String -- receiver
    -> String -- change address
    -> m (Either String (BC.ByteString, PartialMerkleTree, PartialMerkleTree)) -- serialized transaction
getPartiallySignedAllpayTransaction net inputs amount receiverName changeAddr = do
    poolAddr <- poolAddress <$> getNodeConfig
    poolSecKey <- poolSecKey <$> getNodeConfig
    res <- getAddressAndProxyUtxo net receiverName
    let inputsOp =
            (\(op', val) -> (OutPoint (TxHash $ fromString $ opTxHash op') (fromIntegral $ opIndex op'), val)) <$>
            inputs
    case res of
        Left err -> return $ Left $ "failed to get address or proxy-provider utxo: " ++ err
        Right (addr, pputxo, addrProof, utxoProof) -> do
            let ppOutPoint = OutPoint (TxHash $ fromString $ txid $ pputxo) (fromIntegral $ outputIndex $ pputxo)
            let inputs' = ppOutPoint : ((\(outpoint, _) -> outpoint) <$> inputsOp)
            -- compute fee at 5 sat/byte
            let fee = guessTxFee (fromIntegral 5) (1 + length inputs) 2
            -- compute change
            let totalInput = L.foldl (+) 0 $ (\(_, val) -> val) <$> inputsOp
            let change = totalInput - (amount + fromIntegral fee)
            -- add proxy-provider utxo output
            let outputs =
                    [ (DT.pack poolAddr, fromIntegral $ value $ pputxo)
                    , (DT.pack addr, fromIntegral amount)
                    , (DT.pack changeAddr, fromIntegral change)
                    ]
            case buildAddrTx net inputs' outputs of
                Left err -> return $ Left $ "failed to build transaction: " ++ err
                Right tx -> do
                    case decodeOutputBS ((fst . B16.decode) (E.encodeUtf8 $ DT.pack $ scriptPubKey pputxo)) of
                        Left err -> return $ Left $ "failed to decode proxy-provider utxo script: " ++ err
                        Right so -> do
                            let si =
                                    SigInput
                                        so
                                        (fromIntegral $ value pputxo)
                                        ppOutPoint
                                        (setForkIdFlag sigHashAll)
                                        Nothing
                            case signTx net tx [si] [poolSecKey] of
                                Left err -> return $ Left $ "failed to partially sign transaction: " ++ err
                                Right psaTx -> do
                                    let serializedTx = BSL.toStrict $ A.encode $ psaTx
                                    return $ Right (serializedTx, addrProof, utxoProof)

buildProof' :: [TxHash] -> Word32 -> PartialMerkleTree
buildProof' hashes index =
    snd $
    buildPartialMerkle $
    zipWith
        (\h i ->
             if i == index
                 then (h, True)
                 else (h, False))
        hashes
        [0 ..]
