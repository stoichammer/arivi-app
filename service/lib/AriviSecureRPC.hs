-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module AriviSecureRPC
  ( module AriviSecureRPC
        -- ServiceResource(..) ,
        -- globalHandlerRpc,
        -- loopCall
  )
where

import           Arivi.P2P.P2PEnv
-- import Arivi.P2P.RPC.Functions
-- import Arivi.P2P.RPC.Types
import           Arivi.P2P.Types

import           Arivi.P2P.MessageHandler.HandlerTypes
import           Arivi.P2P.PubSub.Class
import           Arivi.P2P.PubSub.Env
import           Arivi.P2P.PubSub.Publish      as Pub
import           Arivi.P2P.PubSub.Types
import           Arivi.P2P.RPC.Env
import           Arivi.P2P.RPC.Fetch
import           Arivi.P2P.Types                ( )

import           Codec.Serialise
import           Control.Monad.IO.Class
import           GHC.Generics
-- import Data.ByteString.Lazy as Lazy
import           Data.Hashable
import           Data.Text.Lazy                as TL
-- import Control.Concurrent    (threadDelay)
-- import           Shared_Types                   ( rPCReq_key
--                                                 , rPCReq_request
--                                                 , RPCResp
--                                                 )
--import Control.Concurrent.MVar
-- import           Data.Int
--import           Data.Map.Strict as M
import           Control.Monad.Reader

import           AriviNetworkServiceHandler
import           Control.Concurrent.Async.Lifted
                                                ( async )
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
--import           Data.Typeable
--type ServiceMsg = Lazy.ByteString
--import Thrift
import           GHC.IO.Handle                 as GH
import           Thrift.Protocol.Binary         ( )
import           Thrift.Server                  ( )
import           Thrift.Transport               ( )
import           Thrift.Protocol.Binary
import           AriviNetworkService_Client    as Client
import           Service_Types                  ( )
import           Data.Set                      as Set
-- import GHC.Stack

data ServiceResource = AriviSecureRPC {
                        --thriftHandle :: BinaryProtocol Handle
                        } --deriving (Generic)
                        deriving (Eq, Ord, Show, Generic)

type ServiceTopic = String

-- data ServiceTopic = HelloWorldHeader
--                    | Dummy_TOPIC
--                    deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceResource
instance Hashable ServiceResource

--instance Serialise ServiceTopic
--instance Hashable ServiceTopic

-- invokeThriftRPC :: ServiceResource -> String -> String
-- invokeThriftRPC resource msg = "sss"
                            --liftIO $ print (typeOf (thriftHandle resource))



-- handler :: (BinaryProtocol Handle) -> ResourceHandler ServiceResource String
-- handler hh = ResourceHandler ( \ (RpcPayload resource serviceMsg) -> RpcPayload resource (invokeThriftRPC resource serviceMsg ))

globalHandlerPubSub :: (HasService env m) => String -> m Status
globalHandlerPubSub msg = do
  val <- asks getThriftConn
  let conn = thriftConn val
  liftIO $ print (msg)
  _x <- liftIO $ Client.notify (conn, conn) (pack msg) (pack msg)
  --liftIO $ print val

  if msg /= "!@#!@#PUB_SUB_HEADER"
    then do
      liftIO (Prelude.putStrLn "Ok")
      -- _ <- async (getHelloWorld msg)
      return Ok
    else liftIO (Prelude.putStrLn "Error") >> return Error

-- getHelloWorld
--   :: (HasP2PEnv env m ServiceResource ServiceTopic String String)
--   => String
--   -> m ()
-- getHelloWorld msg = do
--   resource <- fetchResourceForMessage msg
--                                       (RpcPayload AriviSecureRPC "HelloWorld")
--   liftIO $ print "got resource from notify/publish"
--   liftIO $ print resource


data ThriftEnv = ThriftEnv {
    thriftConn :: BinaryProtocol GH.Handle
} --deriving(Eq, Ord, Show)

class HasThriftEnv env where
    getThriftConn :: env -> ThriftEnv

instance HasThriftEnv (ServiceEnv m r t rmsg pmsg) where
    getThriftConn = thriftEnv

data ServiceEnv m r t rmsg pmsg = ServiceEnv {
      thriftEnv :: ThriftEnv
    , p2pEnv  :: P2PEnv m r t rmsg pmsg
}

type HasService env m =
    ( HasP2PEnv env m ServiceResource ServiceTopic String String
    , HasThriftEnv env
    , MonadReader env m
    )


globalHandlerRpc :: (HasService env m) => String -> m (Maybe String)
globalHandlerRpc msg = do
  env <- asks getThriftConn
  let conn = thriftConn env

  resp <- liftIO $ Client.sendRequest (conn, conn) 0 (pack msg)

  liftIO $ print (msg ++ (show resp))
  return (Just (msg ++ (show resp)))
  -- if msg == "RPC_HEADER"
  --   then return (Just (msg ++ " DUMMY_RESPONSE"))
  --   else return Nothing


-- registerAriviSecureRPC :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerAriviSecureRPC =
--     registerResource AriviSecureRPC handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap AriviSecureRPC

-- getAriviSecureRPC :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => (M.Map Int32 (MVar RPCCall)) -> m ()
-- getAriviSecureRPC mp = do
--     resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
--
--     liftIO $ print "here"
--     liftIO $ print resource

-- theMessage :: Either a (RpcPayload b String) -> Maybe String;
-- theMessage (Right (RpcPayload _ str)) = Just str;
-- theMessage _ = Nothing
stuffPublisher
  :: (HasP2PEnv env m ServiceResource ServiceTopic String String) => m ()
stuffPublisher =
  Pub.publish (PubSubPayload ("HelloWorldTopic", "HelloworldMessage"))


goGetResource
  :: (HasP2PEnv env m ServiceResource ServiceTopic String String)
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
    Right (RpcError _      ) -> liftIO $ print "Exception: RPC error"
    Right (RpcPayload _ str) -> do
      liftIO $ print (str)
      let respMsg = TL.pack str
      liftIO $ (putMVar (response rpcCall) (RPCResp ind respMsg))
      return ()





loopCall
  :: (HasP2PEnv env m ServiceResource ServiceTopic String String)
  => (TChan RPCCall)
  -> m ()
loopCall queue = do

  item     <- liftIO $ atomically $ (readTChan queue)
  -- let x = Set.empty
  -- [Dummy_TOPIC, HelloWorldHeader]
  topicVar <- asks topics
  -- topxx    <- liftIO $ atomically $ readTVar topicVar

  liftIO $ atomically $ modifyTVar' topicVar (Set.insert "Dummy_TOPIC")
  --  atomically $ modifyTVar x (Set.insert nid)


  --Set.insert  x
  --liftIO $ print (typeOf x)
  --let req =  (request item)
  _ <- async (goGetResource item)
  --liftIO $ print (req )
  loopCall queue
  return ()

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
    subscribers = pubSubSubscribers . psEnv. p2pEnv
instance HasNotifiers (ServiceEnv m r t rmsg pmsg) t where
    notifiers = pubSubNotifiers . psEnv . p2pEnv
-- instance HasInbox (ServiceEnv m r t rmsg pmsg) pmsg where
--     inbox = pubSubInbox . psEnv . p2pEnv
-- instance HasCache (ServiceEnv m r t rmsg pmsg) pmsg where
--     cache = pubSubCache . psEnv . p2pEnv
instance HasPubSubEnv (ServiceEnv m r t rmsg pmsg) t  where
    pubSubEnv = psEnv . p2pEnv
instance HasRpcEnv (ServiceEnv m r t rmsg pmsg) r rmsg where
    rpcEnv = rEnv . p2pEnv
instance HasPSGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
    psGlobalHandler = psHandler . p2pEnv
instance HasRpcGlobalHandler (ServiceEnv m r t rmsg pmsg) m r t rmsg pmsg where
    rpcGlobalHandler = rHandler . p2pEnv
