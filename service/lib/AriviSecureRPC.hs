{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module AriviSecureRPC
    ( module AriviSecureRPC
    ) where

import Arivi.P2P.MessageHandler.HandlerTypes (HasNetworkConfig, networkConfig)
import Arivi.P2P.P2PEnv
import Arivi.P2P.PubSub.Class

import Arivi.P2P.PubSub.Publish as Pub
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types hiding (msgType)
import AriviNetworkServiceHandler
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson as A
import Data.Binary as DB
import qualified Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as LBS
import Data.Hashable
import Data.Int
import Data.Map.Strict as M
import Data.Serialize
import Data.Set as Set
import Data.Text as DT
import GHC.Generics
import Network.Simple.TCP as T
import Service.Env
import Service.Types
import System.Random
import Text.Printf

globalHandlerRpc :: (HasService env m) => RPCMessage -> m (Maybe RPCMessage)
globalHandlerRpc msg = throw UnsupportedMethodException

globalHandlerPubSub :: (HasService env m) => String -> PubNotifyMessage -> m Status
globalHandlerPubSub tpc msg = throw UnsupportedMethodException
    -- liftIO $ print ("globalHandlerPubSub")
    -- tcpE <- asks getEndPointEnv
    -- let que = reqQueue tcpE
    -- mid <- liftIO $ randomRIO (1, 268435456)
    -- let req = EndPointMessage mid $ PSN $ Publish' tpc msg
    -- mv <- liftIO $ newEmptyMVar
    -- liftIO $ atomically $ writeTChan que (req, mv)
    -- resp <- liftIO $ takeMVar mv
    -- -- parse this response and either send Ok or Error, Arivi.P2P.PubSub.Types.Error
    -- liftIO $ print (resp)
    -- return (Ok)
