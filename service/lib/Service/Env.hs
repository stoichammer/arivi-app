{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Service.Env where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Reader
import Crypto.Secp256k1
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import Data.IORef
import Data.Map.Strict as M
import Data.Serialize as S
import GHC.Generics
import Network.Simple.TCP as T
import Network.Xoken.Address
import Network.Xoken.Address.Base58
import Network.Xoken.Constants
import Network.Xoken.Crypto.Hash
import Network.Xoken.Keys
import Network.Xoken.Transaction.Common
import NodeConfig as NC
import Service.Data.Utxo
import Service.Types

data XPubInfo =
    XPubInfo
        { key :: XPubKey
        , count :: Int
        , index :: KeyIndex
        , utxoCommitment :: [String]
        } deriving (Show)

decodeXPubInfo :: Network -> ByteString -> Parser XPubInfo
decodeXPubInfo net bs =
    case Data.Aeson.eitherDecode $ BSL.fromStrict bs of
        Right (Object o) ->
            XPubInfo <$> (xPubFromJSON net =<< o .: "key") <*> o .: "count" <*> o .: "index" <*> o .: "utxoCommitment"
        _ -> fail "error while decoding xpubInfo"

encodeXPubInfo :: Network -> XPubInfo -> ByteString
encodeXPubInfo net (XPubInfo k c i u) =
    BSL.toStrict $
    Data.Aeson.encode $ Data.Aeson.object ["key" .= xPubToJSON net k, "count" .= c, "index" .= i, "utxoCommitment" .= u]

getAddressList :: XPubKey -> Int -> [TxHash]
getAddressList pubKey count =
    (TxHash . doubleSHA256 . S.encode . fst . (deriveAddr $ pubKey) ) <$> [1 .. (fromIntegral count)]

outpointHashes :: [String] -> [TxHash]
outpointHashes outpoints = (TxHash . doubleSHA256 . S.encode) <$> outpoints

data ProxyProviderUtxo =
    Unspent
        { address :: String
        , txid :: String
        , outputIndex :: Int
        , value :: Int
        , scriptPubKey :: String
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON ProxyProviderUtxo where
    parseJSON (Object o) =
        (Unspent <$> o .: "address" <*> o .: "txid" <*> o .: "vout" <*> o .: "value" <*> o .: "scriptPubKey")

instance ToJSON ProxyProviderUtxo where
    toJSON (Unspent a t o v s) =
        object ["address" .= a, "txid" .= t, "outputIndex" .= o, "value" .= v, "scriptPubKey" .= s]

class HasNodeConfig m where
    getNodeConfig :: m (NodeConfig)

class HasAddressMap m where
    getAddressMap :: m (TVar (M.Map Base58 [TxHash]))

class HasXPubInfoMap m where
    getXPubHashMap :: m (TVar (M.Map String XPubInfo))

class HasUtxoPool m where
    getUtxoPool :: m (TVar (M.Map String ProxyProviderUtxo))

class HasCommittedUtxos m where
    getCommittedUtxos :: m (TVar (M.Map String ProxyProviderUtxo))

type HasService env m
     = ( MonadReader env m
       , HasNodeConfig m
       , HasAddressMap m
       , HasXPubInfoMap m
       , HasUtxoPool m
       , HasCommittedUtxos m)

data AllpayProxyEnv =
    AllpayProxyEnv
        { nodeConfig :: NodeConfig
        , addressMap :: TVar (M.Map Base58 [TxHash])
        , xpubInfoMap :: TVar (M.Map String XPubInfo)
        , utxoPool :: TVar (M.Map String ProxyProviderUtxo)
        , committedUtxos :: TVar (M.Map String ProxyProviderUtxo)
        }
