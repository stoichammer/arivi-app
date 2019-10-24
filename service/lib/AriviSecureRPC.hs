-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module AriviSecureRPC
  ( module AriviSecureRPC
        -- ServiceResource(..) ,
        -- globalHandlerRpc,
        -- loopCall
  ) where

import Arivi.P2P.P2PEnv

-- import Arivi.P2P.RPC.Functions
-- import Arivi.P2P.RPC.Types
import Arivi.P2P.Types

import Arivi.P2P.MessageHandler.HandlerTypes
import Arivi.P2P.PubSub.Class
import Arivi.P2P.PubSub.Env
import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Env
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types ()
import Data.Aeson as A

import Codec.Serialise
import Control.Monad.IO.Class
import GHC.Generics

import Data.ByteString as BS
import Data.Hashable
import Data.Text.Lazy as TL

import Control.Monad.Reader
import Network.Simple.TCP as T

import AriviNetworkServiceHandler
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Map.Strict as M
import Data.Set as Set

--import STMContainers.Map as HM
import Service_Types ()
import Thrift.Protocol.Binary ()

import Thrift.Server ()
import Thrift.Transport ()

data ServiceResource =
  AriviSecureRPC
    {
    }
  deriving (Eq, Ord, Show, Generic)

type ServiceTopic = String

-- data ServiceTopic = HelloWorldHeader
--                    | Dummy_TOPIC
--                    deriving (Eq, Ord, Show, Generic)
instance Serialise ServiceResource

instance Hashable ServiceResource
                            --liftIO $ print (typeOf (thriftHandle resource))

globalHandlerPubSub :: (HasService env m) => String -> m Status
globalHandlerPubSub msg = do
  val <- asks getTCPEnv
  let _conn = tcpConn val
  liftIO $ print (msg)
  --_x <- liftIO $ Client.notify (conn, conn) (pack msg) (pack msg)
  --liftIO $ print val
  if msg /= "!@#!@#PUB_SUB_HEADER"
    then do
      liftIO (Prelude.putStrLn "Ok")
      -- _ <- async (getHelloWorld msg)
      return Ok
    else liftIO (Prelude.putStrLn "Error") >>
         return Arivi.P2P.PubSub.Types.Error

newtype MsgIdMapper =
  MsgIdMapper (M.Map Int (MVar BS.ByteString))

data TCPEnv =
  TCPEnv
    { tcpConn :: (Socket, SockAddr)
    , reqQueue :: TChan (IPCRequest, (MVar BS.ByteString))
    , msgMatch :: TVar (M.Map Int (MVar BS.ByteString))
    } --deriving(Eq, Ord, Show)

class HasTCPEnv env where
  getTCPEnv :: env -> TCPEnv

instance HasTCPEnv (ServiceEnv m r t rmsg pmsg) where
  getTCPEnv = tcpEnv

data ServiceEnv m r t rmsg pmsg =
  ServiceEnv
    { tcpEnv :: TCPEnv
    , p2pEnv :: P2PEnv m r t rmsg pmsg
    }

type HasService env m
   = ( HasP2PEnv env m ServiceResource ServiceTopic String String
     , HasTCPEnv env
     , MonadReader env m)

globalHandlerRpc :: (HasService env m) => String -> m (Maybe String)
globalHandlerRpc msg = do
  liftIO $ print ("globalHandlerRpc called..")
  tcpE <- asks getTCPEnv
  let que = reqQueue tcpE
  mid <- randomRIO (1, 268435456)
  let req = IPCRequest mid "RPC" (M.singleton "encReq" msg)
  mv <- liftIO $ newEmptyMVar
  liftIO $ atomically $ writeTChan que (req, mv)
  resp1 <- liftIO $ takeMVar mv
  liftIO $ print (show resp1)
  return (Just (show resp1))

processIPCRequests :: (HasService env m) => m ()
processIPCRequests =
  forever $ do
    tcpE <- asks getTCPEnv
    let connSock = tcpConn tcpE
    let que = reqQueue tcpE
    let mm = msgMatch tcpE
    req <- liftIO $ atomically $ readTChan que
    mp <- liftIO $ readTVarIO mm
    let nmp = M.insert (msgid (fst req)) (snd req) mp
    liftIO $ atomically $ writeTVar mm nmp
    T.sendLazy (fst connSock) (A.encode (fst req))
    return ()

processIPCResponses :: (HasService env m) => m ()
processIPCResponses =
  forever $ do
    tcpE <- asks getTCPEnv
    let connSock = tcpConn tcpE
    let mm = msgMatch tcpE
    mp <- liftIO $ readTVarIO mm
    bytes <- liftIO $ T.recv (fst connSock) 1024
    case bytes of
      Just x -> do
        liftIO $ print (show x)
        let mv = M.lookup 123 mp
        case mv of
          Just a -> liftIO $ putMVar a x
          Nothing -> liftIO $ print ("HM lookup failed.")
      Nothing -> liftIO $ print ("Nothing.")
    return ()

-- registerAriviSecureRPC :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerAriviSecureRPC =
--     registerResource AriviSecureRPC handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap AriviSecureRPC
stuffPublisher ::
     (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
stuffPublisher =
  Pub.publish (PubSubPayload ("HelloWorldTopic", "HelloworldMessage"))

goGetResource ::
     (HasP2PEnv env m ServiceResource ServiceTopic String String)
  => RPCCall
  -> m ()
goGetResource rpcCall = do
  let req = (request rpcCall)
  let ind = rPCReq_key req
  let msg = show (rPCReq_request req)
  liftIO $ print ("fetchResource")
  resource <- fetchResource (RpcPayload AriviSecureRPC msg)
  --liftIO $ print (typeOf resource)
  --liftIO $ print (theMessage resource)
  case resource of
    Left _ -> do
      liftIO $ print "Exception: No peers available to issue RPC"
      let errMsg = TL.pack "__EXCEPTION__NO_PEERS"
      liftIO $ (putMVar (response rpcCall) (RPCResp ind errMsg))
      return ()
    Right (RpcError _) -> liftIO $ print "Exception: RPC error"
    Right (RpcPayload _ str) -> do
      liftIO $ print (str)
      let respMsg = TL.pack str
      liftIO $ (putMVar (response rpcCall) (RPCResp ind respMsg))
      return ()

loopRPC ::
     (HasP2PEnv env m ServiceResource ServiceTopic String String)
  => (TChan RPCCall)
  -> m ()
loopRPC queue =
  forever $ do
    item <- liftIO $ atomically $ (readTChan queue)
    _ <- async (goGetResource item)
      --liftIO $ print (req )
    return ()

pubSubMsgType :: PubSubMsg -> [Char]
pubSubMsgType (Subscribe1 _t) = "SUBSCRIBE"
pubSubMsgType (Publish1 _t _m) = "PUBLISH"
pubSubMsgType (Notify1 _t _m) = "NOTIFY"

loopPubSub ::
     (HasP2PEnv env m ServiceResource ServiceTopic String String)
  => (TChan PubSubMsg)
  -> m ()
loopPubSub queue =
  forever $ do
    item <- liftIO $ atomically $ (readTChan queue)
    liftIO $ print ("read something..")
    case (pubSubMsgType item) of
      "SUBSCRIBE" -> do
        topicVar <- asks topics
        liftIO $ atomically $ modifyTVar' topicVar (Set.insert (topic item))
      "PUBLISH" -> do
        liftIO $ print ("PUBLISH")
        Pub.publish (PubSubPayload ((topic item), (message item)))
      "NOTIFY" -> undefined
      __ -> undefined
      --loopPubSub queue

instance HasNetworkConfig (ServiceEnv m r t rmsg pmsg) NetworkConfig where
  networkConfig f se =
    fmap
      (\nc ->
         se
           { p2pEnv =
               (p2pEnv se)
                 { nodeEndpointEnv =
                     (nodeEndpointEnv (p2pEnv se))
                       {Arivi.P2P.P2PEnv._networkConfig = nc}
                 }
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
