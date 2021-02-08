{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Subscriber where

import Codec.Serialise
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Crypto.Secp256k1
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString as B
import Data.ByteString.Base16 as B16
import Data.ByteString.Builder
import Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Int
import Data.Map as M
import Data.Maybe
import Data.Serialize
import Data.Text as DT
import Data.Text.Encoding as DTE
import GHC.Generics
import LevelDB
import Network.Xoken.Address
import Network.Xoken.Constants
import Network.Xoken.Crypto
import Network.Xoken.Keys
import Network.Xoken.Util
import NodeConfig as NC
import Service.Data.Allegory as AL
import Service.Env
import Service.Merkle
import Service.ProxyProviderUtxo
import Service.Types

addSubscriber :: (HasService env m, MonadIO m) => [Int] -> C8.ByteString -> String -> Int -> m C8.ByteString
addSubscriber name xPubKey ownerUri count = do
    net <- NC.bitcoinNetwork <$> getNodeConfig
    subs <- getSubscribers
    case parse Prelude.id . xPubFromJSON net $ A.String (DTE.decodeUtf8 xPubKey) of
        Error e -> throw $ XPubKeyDecodeException e
        Success key -> do
            committedUtxos <- getFromPool count
            let utxoHashes = doubleSHA256 . C8.pack <$> committedUtxos
                utxoMT = buildMerkleTree utxoHashes
                addrHashes =
                    doubleSHA256 . C8.pack . DT.unpack . fromJust . addrToString net <$> generateAddresses key count
                addressMT = buildMerkleTree addrHashes
                expiry = 2556143999
            (opRetData, opRetHash) <-
                makeOpReturn
                    name
                    ownerUri
                    (C8.unpack . hash256ToHex $ getMerkleRoot addressMT)
                    (C8.unpack . hash256ToHex $ getMerkleRoot addressMT)
                    expiry
            let opRetHashStr = C8.unpack . hash256ToHex $ opRetHash
                subscriberRecord = Subscriber key count 0 committedUtxos addressMT utxoMT False
                f x =
                    case x of
                        Just v -> Just v
                        Nothing -> Just subscriberRecord
            subMap <- liftIO $ readTVarIO subs
            when (isNothing $ M.lookup opRetHashStr subMap) $
                liftIO $ putValue (DTE.encodeUtf8 $ DT.pack opRetHashStr) (encodeSubscriber net subscriberRecord)
            liftIO $ atomically $ writeTVar subs (M.alter f opRetHashStr subMap)
            return opRetData

cancelSubscription :: (HasService env m, MonadIO m) => Hash256 -> m ()
cancelSubscription opRetHash = do
    nodeCnf <- getNodeConfig
    let net = NC.bitcoinNetwork nodeCnf
    subs <- getSubscribers
    subMap <- liftIO $ readTVarIO subs
    let opRetHashStr = C8.unpack . hash256ToHex $ opRetHash
        committedUtxos = ppUtxos . fromJust $ M.lookup opRetHashStr subMap
    -- delete subscription record
    liftIO $ atomically $ modifyTVar subs $ M.delete opRetHashStr
    liftIO $ deleteValue (DTE.encodeUtf8 $ DT.pack opRetHashStr)
    -- free up committed utxos; return them to pool
    putBackInPool committedUtxos
    return ()

generateAddresses :: XPubKey -> Int -> [Address]
generateAddresses xPubKey count = (fst . (deriveAddr $ xPubKey)) <$> [1 .. (fromIntegral count)]

makeOpReturn ::
       (HasService env m, MonadIO m) => [Int] -> String -> String -> String -> Int64 -> m (C8.ByteString, Hash256)
makeOpReturn allegoryName ownerUri addrCom utxoCom expiry = do
    providerUri <- NC.proxyProviderUri <$> getNodeConfig
    let al =
            Allegory
                1
                allegoryName
                (OwnerAction
                     (AL.Index 0)
                     (OwnerOutput (AL.Index 1) (Just $ Endpoint "XokenP2P" ownerUri))
                     [ ProxyProvider
                           "AllPay"
                           "Public"
                           (Endpoint "XokenP2P" providerUri)
                           (AL.Registration addrCom utxoCom "" (fromIntegral $ expiry))
                     ])
        opRetScript = frameOpReturn $ LC.toStrict $ serialise al
        opRetHash = sha256 $ DTE.encodeUtf8 $ encodeHex opRetScript
    return (opRetScript, opRetHash)

frameOpReturn :: C8.ByteString -> C8.ByteString
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
    C8.append (C8.append prefix bs) opReturn
