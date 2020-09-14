{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Env where

import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
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
import Service.Data
import Service.Data.Utxo
import Service.Types
import UtxoPool

data ServiceResource =
    AriviSecureRPC
        {
        }
    deriving (Eq, Ord, Show, Generic)

type ServiceTopic = String

instance Serialise ServiceResource

instance Hashable ServiceResource

data XPubInfo =
    XPubInfo
        { key :: XPubKey
        , count :: Int
        , index :: KeyIndex
        , utxoCommitment :: [String]
        }

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
    (TxHash . doubleSHA256 . S.encode . xPubAddr . pubSubKey pubKey) <$> [1 .. (fromIntegral count)]

outpointHashes :: [String] -> [TxHash]
outpointHashes outpoints = (TxHash . doubleSHA256 . S.encode) <$> outpoints

data EndPointEnv =
    EndPointEnv
        -- tcpConn :: (Socket, SockAddr)
        -- , reqQueue :: TChan (EndPointMessage, (MVar EndPointMessage))
        -- , msgMatch :: TVar (M.Map Int (MVar EndPointMessage))
        {
        }

class HasEndPointEnv env where
    getEndPointEnv :: env -> EndPointEnv

instance HasEndPointEnv (ServiceEnv m r t rmsg pmsg) where
    getEndPointEnv = tcpEnv

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

data ServiceEnv m r t rmsg pmsg =
    ServiceEnv
        { tcpEnv :: EndPointEnv
        , p2pEnv :: P2PEnv m r t rmsg pmsg
        , nodeConfig :: NodeConfig
        , addressMap :: TVar (M.Map Base58 [TxHash])
        , xpubInfoMap :: TVar (M.Map String XPubInfo)
        , utxoPool :: TVar (M.Map String ProxyProviderUtxo)
        , committedUtxos :: TVar (M.Map String ProxyProviderUtxo)
        }

type HasService env m
     = ( HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage
       , HasEndPointEnv env
       , MonadReader env m
       , HasNodeConfig m
       , HasAddressMap m
       , HasXPubInfoMap m
       , HasUtxoPool m
       , HasCommittedUtxos m)

instance HasNetworkConfig (ServiceEnv m r t rmsg pmsg) NetworkConfig where
    networkConfig f se =
        fmap
            (\nc ->
                 se
                     { p2pEnv =
                           (p2pEnv se)
                               {nodeEndpointEnv = (nodeEndpointEnv (p2pEnv se)) {Arivi.P2P.P2PEnv._networkConfig = nc}}
                     })
            (f ((Arivi.P2P.P2PEnv._networkConfig . nodeEndpointEnv . p2pEnv) se))

instance HasTopics (ServiceEnv m r t rmsg pmsg) t where
    topics = pubSubTopics . psEnv . p2pEnv

instance HasSubscribers (ServiceEnv m r t rmsg pmsg) t where
    subscribers = pubSubSubscribers . psEnv . p2pEnv

instance HasNotifiers (ServiceEnv m r t rmsg pmsg) t where
    notifiers = pubSubNotifiers . psEnv . p2pEnv

instance HasPubSubEnv (ServiceEnv m r t rmsg pmsg) t where
    pubSubEnv = psEnv . p2pEnv

instance HasRpcEnv (ServiceEnv m r t rmsg pmsg) r rmsg where
    rpcEnv = rEnv . p2pEnv

instance HasPSGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
    psGlobalHandler = psHandler . p2pEnv

instance HasRpcGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
    rpcGlobalHandler = rHandler . p2pEnv
