{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  ( module Main
  ) where

import AriviSecureRPC

import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Env
import Arivi.Network
import Arivi.P2P
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.P2PEnv as PE
import Arivi.P2P.ServiceRegistry

import Arivi.P2P.PubSub.Types

-- import           Arivi.P2P.Types
-- import           Arivi.P2P.Handler  (newIncomingConnectionHandler)
-- import           Arivi.P2P.Kademlia.LoadDefaultPeers
-- import           Arivi.P2P.MessageHandler.HandlerTypes
-- import           Arivi.P2P.PubSub.Env
-- import           Arivi.P2P.PubSub.Class
import Arivi.P2P.RPC.Types

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Monad.Logger
import Control.Monad.Reader
import Data.ByteString.Lazy as BSL (ByteString)
import Data.ByteString.Lazy.Char8 as BSLC (pack)
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.String.Conv
import Data.Text
import Data.Typeable

import System.Directory (doesPathExist)
import System.Environment (getArgs)

--import           Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM

--import ThriftServer
import qualified AriviNetworkService
import AriviNetworkServiceHandler
import AriviNetworkService_Iface
import Service_Types

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Trans.Control
import Data.Int

--import           SharedService_Iface           as SharedIface
import Shared_Types

newtype AppM a =
  AppM
    (ReaderT (ServiceEnv AppM ServiceResource ServiceTopic String String) (LoggingT IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (ServiceEnv AppM ServiceResource ServiceTopic String String)
           , MonadIO
           , MonadThrow
           , MonadCatch
           , MonadLogger
           )

deriving instance MonadBase IO AppM

deriving instance MonadBaseControl IO AppM

instance HasNetworkEnv AppM where
  getEnv = asks (ariviNetworkEnv . nodeEndpointEnv . p2pEnv)

instance HasSecretKey AppM

instance HasKbucket AppM where
  getKb = asks (kbucket . kademliaEnv . p2pEnv)

instance HasStatsdClient AppM where
  getStatsdClient = asks (statsdClient . p2pEnv)

instance HasNodeEndpoint AppM where
  getEndpointEnv = asks (nodeEndpointEnv . p2pEnv)
  getNetworkConfig = asks (PE._networkConfig . nodeEndpointEnv . p2pEnv)
  getHandlers = asks (handlers . nodeEndpointEnv . p2pEnv)
  getNodeIdPeerMapTVarP2PEnv =
    asks (tvarNodeIdPeerMap . nodeEndpointEnv . p2pEnv)

instance HasPRT AppM where
  getPeerReputationHistoryTableTVar =
    asks (tvPeerReputationHashTable . prtEnv . p2pEnv)
  getServicesReputationHashMapTVar =
    asks (tvServicesReputationHashMap . prtEnv . p2pEnv)
  getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv . p2pEnv)
  getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv . p2pEnv)
  getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv . p2pEnv)

-- instance HasNetworkConfig (P2PEnv r t rmsg pmsg) NetworkConfig where
--     networkConfig f p2p =
--         fmap
--             (\nc ->
--                  p2p
--                  { nodeEndpointEnv =
--                        (nodeEndpointEnv p2p) {PE._networkConfig = nc}
--                  })
--             (f ((PE._networkConfig . nodeEndpointEnv) p2p))
-- instance HasArchivedResourcers AppM ServiceResource String where
--     archived = asks (tvarArchivedResourceToPeerMap . rpcEnv)
--
-- instance HasTransientResourcers AppM ServiceResource String where
--     transient = asks (tvarDynamicResourceToPeerMap . rpcEnv)
--
--
-- instance HasPRT AppM where
--     getPeerReputationHistoryTableTVar = asks (tvPeerReputationHashTable . prtEnv . p2pEnv)
--     getServicesReputationHashMapTVar = asks (tvServicesReputationHashMap . prtEnv . p2pEnv)
--     getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv . p2pEnv)
--     getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv . p2pEnv)
--     getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv . p2pEnv)
-- instance HasTopics (P2PEnv r t rmsg pmsg) t where
--     topics = pubSubTopics . psEnv
-- instance HasSubscribers (P2PEnv r t rmsg pmsg) t where
--     subscribers = pubSubSubscribers . psEnv
-- instance HasNotifiers (P2PEnv r t rmsg pmsg) t where
--     notifiers = pubSubNotifiers . psEnv
-- instance HasInbox (P2PEnv r t rmsg pmsg) pmsg where
--     inbox = pubSubInbox . psEnv
-- instance HasCache (P2PEnv r t rmsg pmsg) pmsg where
--     cache = pubSubCache . psEnv
-- instance HasTopicHandlers (P2PEnv r t rmsg pmsg) t pmsg where
--     topicHandlers = pubSubHandlers . psEnv
-- instance HasPubSubEnv (P2PEnv ServiceResource ByteString String ByteString) ByteString ByteString where
--     pubSubEnv = psEnv
runAppM ::
     ServiceEnv AppM ServiceResource ServiceTopic String String
  -> AppM a
  -> LoggingT IO a
runAppM env (AppM app) = runReaderT app env

defaultConfig :: FilePath -> IO ()
defaultConfig path = do
  (sk, _) <- ACUPS.generateKeyPair
  let config =
        Config.Config
          5678
          5678
          sk
          []
          (generateNodeId sk)
          "127.0.0.1"
          (Data.Text.pack (path <> "/node.log"))
          20
          5
          3
          9090
          9091
  Config.makeConfig config (path <> "/config.yaml")

runNode :: Config.Config -> AriviNetworkServiceHandler -> IO ()
runNode config ariviHandler
    -- config <- Config.readConfig configPath
 = do
  env <-
    mkP2PEnv config globalHandlerRpc globalHandlerPubSub [AriviSecureRPC] []
  let something = ThriftEnv (binProto ariviHandler)
  let serviceEnv = ServiceEnv something env
  runFileLoggingT (toS $ Config.logFile config) $
    runAppM
      serviceEnv
      (do initP2P config
          liftIO $ threadDelay 5000000
      --t1 <- async (loopRPC (rpcQueue ariviHandler))
      --t2 <- async (loopPubSub (pubSubQueue ariviHandler))
      --wait t1
      --wait t2
          loopPubSub (pubSubQueue ariviHandler)
      -- stuffPublisher
      --liftIO $ threadDelay 500000000
       )
  return ()

main :: IO ()
main = do
  (path:_) <- getArgs
  b <- doesPathExist (path <> "/config.yaml")
  unless b (defaultConfig path)
  config <- Config.readConfig (path <> "/config.yaml")
  -- ariviHandler <- newAriviNetworkServiceHandler (Config.thriftRemotePort config)
  ariviHandler <-
    setupThriftDuplex
      (Config.thriftListenPort config)
      (Config.thriftRemotePort config)
  --let mv    = ariviThriftLog ariviHandler
  --let queue = rpcQueue ariviHandler
  --print (typeOf mv)
  --print (typeOf queue)
  --mp <- readMVar queue
  --print (size mp)
  -- st <-  SharedIface.getStruct handler 0
  -- print (typeOf  st)
  -- print (sharedStruct_key st)
  -- print (sharedStruct_value st)
  --
  --let flag = M.member (SharedStruct 1 "hello") xx
  --print (flag)
  --M.insert (SharedStruct 1 "hello") xx
  --async (setupThriftDuplex ariviHandler (Config.thriftListenPort config))
  runNode config ariviHandler
  return ()

a :: Prelude.Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')

myAmazingHandler :: (HasLogging m) => ConnectionHandle -> m ()
myAmazingHandler h = forever $ recv h >>= send h
