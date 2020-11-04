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

module Service.Registration where

import Codec.Serialise
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString as B
import Data.Serialize
import Data.ByteString.Base16 as B16
import Data.ByteString.Builder
import Data.ByteString.Char8 as C
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as LC
import Data.Map.Strict as M
import Data.String (IsString, fromString)
import qualified Data.Text as DT
import Data.Text.Encoding as DTE

import Service.AllpayTransaction
import Service.Data
import Service.Data.Allegory
import Service.Env
import Service.ProxyProviderUtxo
import Service.Types
import UtxoPool

import Network.Xoken.Block.Merkle
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Transaction.Common
import Network.Xoken.Address

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Network.Xoken.Constants

import Data.Int
import LevelDB

data RegDetails =
    RegDetails
        { addrCom :: String
        , utxoCom :: String
        , exp' :: Int64
        }
    deriving (Show, Eq)

-- create: addressCommitment, utxoCommitment, list of pputxo outpoints and address hashes
getRegistrationDetails ::
       (HasService env m, MonadIO m)
    => Network
    -> C.ByteString
    -> Int
    -> m (Either String (RegDetails, [String], [TxHash], XPubKey))
getRegistrationDetails net pubKey count = do
    let res = Data.Aeson.String (DTE.decodeUtf8 pubKey)
    case parse Prelude.id . xPubFromJSON net $ res of
        Success k -> do
            ops <- getFromPool count
            let addrHashes = getAddressList k count
            let utxoRoot = buildMerkleRoot $ outpointHashes ops
            let addrRoot = buildMerkleRoot $ addrHashes
            let expiry = 2556143999 -- for demo, midnight 31st December 2050
            return $ Right $ (RegDetails (show addrRoot) (show utxoRoot) expiry, ops, addrHashes, k)
        Error err -> do
            return $ Left $ "error occured while decoding xpubkey: " <> err

registerNewUser ::
       (HasService env m, MonadIO m)
    => Network
    -> [Int]
    -> C.ByteString
    -> Int
    -> (OutPoint', Int64)
    -> String
    -> m (Either String C.ByteString)
registerNewUser net allegoryName pubKey count (nutxo, value) returnAddr = do
    aMapTvar <- getXPubHashMap
    addressTVar <- getAddressMap
    regDetails <- getRegistrationDetails net pubKey count
    case regDetails of
        Left err -> return $ Left $ "error while procuring registration details: " <> err
        Right (reg, committedOps, addrHashes, k) -> do
            mkTxRes <- makeRegistrationTx net (nutxo, value) allegoryName returnAddr reg
            case mkTxRes of
                Left err -> return $ Left $ "error while making registration transaction: " <> err
                Right (stx, opRetHash) -> do
                    xPubInfo <- liftIO $ readTVarIO aMapTvar
                    let f x =
                            case x of
                                Just v -> Just v
                                Nothing -> Just (XPubInfo k count 0 committedOps)
                    liftIO $ atomically $ writeTVar aMapTvar (M.alter f (show opRetHash) xPubInfo)
                    liftIO $ atomically $ modifyTVar addressTVar (M.insert (xPubExport net k) addrHashes)
                    names <- liftIO $ M.keys <$> readTVarIO aMapTvar
                    liftIO $ putValue "names" (BSL.toStrict $ Data.Aeson.encode $ nub $ (show opRetHash) : names)
                    when (isNothing $ M.lookup (show opRetHash) xPubInfo) $
                        liftIO $ putValue (DTE.encodeUtf8 $ DT.pack (show opRetHash)) (encodeXPubInfo net $ XPubInfo k count 0 [])
                    liftIO $ print $ "HASH TO USE***: " <> (show opRetHash)
                    return $ Right $ stx

ref12345 net pubKey count name = do
    aMapTvar <- getXPubHashMap
    addressTVar <- getAddressMap
    let res = Data.Aeson.String (DTE.decodeUtf8 pubKey)
    case parse Prelude.id . xPubFromJSON net $ res of
        Success k -> do
            committedOutpoints <- getFromPool count
            let utxoRoot = buildMerkleRoot $ outpointHashes committedOutpoints
            let addrRoot = buildMerkleRoot $ (getAddressList k count)
            xPubInfo <- liftIO $ readTVarIO aMapTvar
            let f x =
                    case x of
                        Just v -> Just v
                        Nothing -> Just (XPubInfo k count 0 committedOutpoints)
            liftIO $ atomically $ writeTVar aMapTvar (M.alter f name xPubInfo)
            liftIO $ atomically $ modifyTVar addressTVar (M.insert (xPubExport net k) (getAddressList k count))
            names <- liftIO $ M.keys <$> readTVarIO aMapTvar
            liftIO $ putValue "names" (BSL.toStrict $ Data.Aeson.encode $ nub $ name : names)
            when (isNothing $ M.lookup name xPubInfo) $
                liftIO $ putValue (DTE.encodeUtf8 $ DT.pack name) (encodeXPubInfo net $ XPubInfo k count 0 [])
            return $ Right $ (True, (show utxoRoot), (show addrRoot))
        Error err -> do
            return $ Left $ "error occurred while decoding XPubKey: " <> show err

makeRegistrationTx ::
       (HasService env m, MonadIO m)
    => Network
    -> (OutPoint', Int64) -- name-utxo input, value
    -> [Int] -- allegory name
    -> String -- return address
    -> RegDetails -- registration details
    -> m (Either String (C.ByteString, Hash256))
makeRegistrationTx net nutxoInput allegoryName retAddr reg = do
    case stringToAddr net (DT.pack retAddr) of
        Nothing -> return $ Left "failed to decode return address"
        (Just addr) -> do
            let nUtxoIp = (\(op', val) -> (TxIn (OutPoint (TxHash $ fromString $ opTxHash op') (fromIntegral $ opIndex op')) "" 0)) nutxoInput
            let nUtxoOp = (TxOut (fromIntegral $ snd nutxoInput) (addressToScriptBS $ addr))
            let al =
                    Allegory
                        1
                        allegoryName
                        (OwnerAction
                            (Index 0)
                            (OwnerOutput (Index 1) (Just $ Endpoint "XokenP2P" "someuri"))
                            [ ProxyProvider
                                "AllPay"
                                "Public"
                                (Endpoint "XokenP2P" "uri2")
                                (Registration (addrCom reg) (utxoCom reg) "" (fromIntegral $ exp' reg))
                            ])
            let opRetScript = frameOpReturn $ LC.toStrict $ serialise al
            let opRetHash = sha256 opRetScript
            let inputs = [nUtxoIp]
            let outputs = (TxOut 0 opRetScript) : nUtxoOp : []
            let tx = Tx 1 inputs outputs 0
            let stx = BSL.toStrict $ Data.Aeson.encode tx
            return $ Right (stx, opRetHash)

frameOpReturn :: C.ByteString -> C.ByteString
frameOpReturn opReturn = do
    let prefix = (fst . B16.decode) "006a0f416c6c65676f72792f416c6c506179"
    let len = B.length opReturn
    let xx =
            if (len <= 0x4b)
                then word8 $ fromIntegral len
                else if (len <= 0xff)
                         then mappend (word8 0x4c) (word8 $ fromIntegral len)
                         else if (len <= 0xffff)
                                  then mappend (word8 0x4d) (word16LE $ fromIntegral len)
                                  else if (len <= 0x7fffffff)
                                           then mappend (word8 0x4e) (word32LE $ fromIntegral len)
                                           else word8 0x99 -- error scenario!!
    let bs = LC.toStrict $ toLazyByteString xx
    C.append (C.append prefix bs) opReturn
