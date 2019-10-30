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

import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Env
import Arivi.Network
import Arivi.P2P
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.P2PEnv as PE
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Types
import Arivi.P2P.ServiceRegistry
import AriviNetworkServiceHandler
import AriviSecureRPC
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL (ByteString)
import Data.ByteString.Lazy.Char8 as BSLC (pack)
import Data.Int
import Data.Map.Strict as M
import Data.Monoid ((<>))
import Data.String.Conv
import Data.Text
import Data.Typeable
import Network.Simple.TCP
import STMContainers.Map as H
import System.Directory (doesPathExist)
import System.Environment (getArgs)

newtype AppM a =
    AppM (ReaderT (ServiceEnv AppM ServiceResource ServiceTopic String String) (LoggingT IO) a)
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
    getNodeIdPeerMapTVarP2PEnv = asks (tvarNodeIdPeerMap . nodeEndpointEnv . p2pEnv)

instance HasPRT AppM where
    getPeerReputationHistoryTableTVar = asks (tvPeerReputationHashTable . prtEnv . p2pEnv)
    getServicesReputationHashMapTVar = asks (tvServicesReputationHashMap . prtEnv . p2pEnv)
    getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv . p2pEnv)
    getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv . p2pEnv)
    getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv . p2pEnv)

runAppM :: ServiceEnv AppM ServiceResource ServiceTopic String String -> AppM a -> LoggingT IO a
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
    p2pEnv <- mkP2PEnv config globalHandlerRpc globalHandlerPubSub [AriviSecureRPC] []
    let rPort = Config.thriftRemotePort config
    sockTuple <- connectSock "127.0.0.1" (show rPort)
    Prelude.putStrLn $ "Connection established to " ++ show rPort
    que <- atomically $ newTChan
    mmap <- newTVarIO $ M.empty
    let tcpEnv = TCPEnv sockTuple que mmap
    let serviceEnv = ServiceEnv tcpEnv p2pEnv
    runFileLoggingT (toS $ Config.logFile config) $
        runAppM
            serviceEnv
            (do initP2P config
                liftIO $ threadDelay 5000000
                t1 <- async (loopRPC (rpcQueue ariviHandler))
                t2 <- async (loopPubSub (pubSubQueue ariviHandler))
                async (processIPCRequests)
                async (processIPCResponses)
          -- wait t1
          -- wait t2
                liftIO $ threadDelay 99999999000000000)
    return ()

main :: IO ()
main = do
    (path:_) <- getArgs
    b <- doesPathExist (path <> "/config.yaml")
    unless b (defaultConfig path)
    config <- Config.readConfig (path <> "/config.yaml")
    ariviHandler <- newAriviNetworkServiceHandler
    _ <- async (setupIPCServer ariviHandler (Config.thriftListenPort config))
    runNode config ariviHandler
    return ()

a :: Prelude.Int -> BSL.ByteString
a n = BSLC.pack (Prelude.replicate n 'a')
-- myAmazingHandler :: (HasLogging m) => ConnectionHandle -> m ()
-- myAmazingHandler h = forever $ recv h >>= send h
