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

import Codec.Serialise
import Control.Monad.IO.Class
import GHC.Generics

-- import Data.ByteString.Lazy as Lazy
import Data.Hashable
import Data.Text.Lazy as TL

import Control.Monad.Reader
import Network.Simple.TCP

import AriviNetworkServiceHandler
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM

import Data.Set as Set

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
  val <- asks getTCPConn
  let _conn = tcpConn val
  liftIO $ print (msg)
  --_x <- liftIO $ Client.notify (conn, conn) (pack msg) (pack msg)
  --liftIO $ print val
  if msg /= "!@#!@#PUB_SUB_HEADER"
    then do
      liftIO (Prelude.putStrLn "Ok")
      -- _ <- async (getHelloWorld msg)
      return Ok
    else liftIO (Prelude.putStrLn "Error") >> return Error

data TCPEnv =
  TCPEnv
    { tcpConn :: (Socket, SockAddr)
    } --deriving(Eq, Ord, Show)

class HasTCPEnv env where
  getTCPConn :: env -> TCPEnv

instance HasTCPEnv (ServiceEnv m r t rmsg pmsg) where
  getTCPConn = tcpEnv

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
  env <- asks getTCPConn
  let _conn = tcpConn env
  --resp <- liftIO $ Client.sendRequest (conn, conn) 0 (pack msg)
  --liftIO $ print (msg ++ (show resp))
  return (Just (msg))
  -- if msg == "RPC_HEADER"
  --   then return (Just (msg ++ " DUMMY_RESPONSE"))
  --   else return Nothing

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
  liftIO $ print (msg)
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
