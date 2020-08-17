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

module Service where

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
import Data.Word (Word32, Word8)
import Network.Xoken.Address
import Network.Xoken.Block.Merkle
import Network.Xoken.Transaction.Common

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

--import qualified Data.ByteString.UTF8 as BSU (toString)
import Data.Aeson
import Data.Aeson.Types
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
import Network.Xoken.Keys
import Service.Data
import Service.Env
import Service.ProxyProviderUtxo
import Service.Types

import Service.AllpayTransaction

data ServiceException
    = KeyValueDBLookupException
    | GraphDBLookupException
    | InvalidOutputAddressException
    deriving (Show)

instance Exception ServiceException

goGetResource :: (HasService env m, MonadIO m) => RPCMessage -> Network -> m (RPCMessage)
goGetResource msg net = do
    aMapTvar <- getXPubHashMap
    addressTVar <- getAddressMap
    case rqMethod msg of
        "ADD_XPUBKEY" -> do
            case rqParams msg of
                Just (AddXPubKey pubKey count name) -> do
                    let res = Data.Aeson.String (DTE.decodeUtf8 pubKey)
                    case parse Prelude.id . xPubFromJSON net $ res of
                        Success k -> do
                            committedOutpoints <- getFromPool count
                            let root = buildMerkleRoot $ outpointHashes committedOutpoints
                            xPubInfo <- liftIO $ readTVarIO aMapTvar
                            let f x =
                                    case x of
                                        Just v -> Just v
                                        Nothing -> Just (XPubInfo k count 0 committedOutpoints)
                            liftIO $ atomically $ writeTVar aMapTvar (M.alter f name xPubInfo)
                            liftIO $
                                atomically $
                                modifyTVar addressTVar (M.insert (xPubExport net k) (getAddressList k count))
                            names <- liftIO $ M.keys <$> readTVarIO aMapTvar
                            liftIO $ putValue "names" (BSL.toStrict $ Data.Aeson.encode $ nub $ name : names)
                            when (isNothing $ M.lookup name xPubInfo) $
                                liftIO $
                                putValue (DTE.encodeUtf8 $ DT.pack name) (encodeXPubInfo net $ XPubInfo k count 0 [])
                            return $ RPCResponse 200 Nothing (Just $ RespXPubKey True)
                        Error err -> do
                            liftIO $ print $ "error occurred while decoding XPubKey: " <> show err
                            return $ RPCResponse 400 (Just INVALID_REQUEST) Nothing
                Nothing -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "NAME->ADDR" ->
            case rqParams msg of
                Just (GetNextAddress name) -> do
                    aMap <- liftIO $ readTVarIO aMapTvar
                    case M.lookup name aMap of
                        Just XPubInfo {..} -> do
                            if count > fromIntegral index
                                then do
                                    let addr = xPubAddr (pubSubKey key (index + 1))
                                    let ppo = utxoCommitment !! (fromIntegral index + 1)
                                    pputxo <- getCommittedUtxo ppo
                                    liftIO $
                                        atomically $
                                        writeTVar
                                            aMapTvar
                                            (M.update (\(XPubInfo k c i u) -> Just $ XPubInfo k c (i + 1) u) name aMap)
                                    addressMap <- liftIO $ readTVarIO addressTVar
                                    case M.lookup (xPubExport net key) addressMap of
                                        Just hashes -> do
                                            let merkleProof = buildProof hashes index
                                            liftIO $
                                                putValue
                                                    (DTE.encodeUtf8 $ DT.pack name)
                                                    (encodeXPubInfo net $ XPubInfo key count (index + 1) utxoCommitment)
                                            return $
                                                RPCResponse
                                                    200
                                                    Nothing
                                                    (Just $
                                                     RespGetNextAddress
                                                         (DT.unpack $ fromJust $ addrToString net addr)
                                                         merkleProof
                                                         (fromJust pputxo))
                                        Nothing -> do
                                            liftIO $ print "no data found in address map"
                                            return $ RPCResponse 400 (Just INVALID_REQUEST) Nothing
                                else do
                                    liftIO $ print "maximum address count reached"
                                    return $ RPCResponse 400 (Just INVALID_REQUEST) Nothing
                        Nothing -> return $ RPCResponse 400 (Just INVALID_REQUEST) Nothing
                Nothing -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        "PS_ALLPAY_TX" ->
            case rqParams msg of
                Just (PSAllpayTransaction inputs recipient amount change) -> do
                    liftIO $ print $ "PS_ALLPAY_TX request received!"
                    liftIO $ print $ "params: inputs: " ++ (show inputs) ++ ", recipient: " ++ (show recipient)
                    liftIO $ print $ "amount: " ++ (show amount) ++ ", change address: " ++ (show change)
                    res <- getPartiallySignedAllpayTransaction net inputs amount recipient change
                    liftIO $ print $ show res
                    return $ RPCResponse 200 Nothing Nothing
                Nothing -> return $ RPCResponse 400 (Just INVALID_PARAMS) Nothing
        _____ -> return $ RPCResponse 400 (Just INVALID_METHOD) Nothing

buildProof :: [TxHash] -> Word32 -> PartialMerkleTree
buildProof hashes index =
    snd $
    buildPartialMerkle $
    zipWith
        (\h i ->
             if i == index
                 then (h, True)
                 else (h, False))
        hashes
        [0 ..]
