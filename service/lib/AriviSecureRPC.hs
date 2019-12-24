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
    -- tcpE <- asks getEndPointEnv
    -- let que = reqQueue tcpE
    -- mid <- liftIO $ randomRIO (1, 268435456)
    -- let req = EndPointMessage mid $ RPC msg
    -- mv <- liftIO $ newEmptyMVar
    -- liftIO $ atomically $ writeTChan que (req, mv)
    -- resp <- liftIO $ takeMVar mv
    -- case payload resp of
    --     RPC r -> do
    --         liftIO $ print (r)
    --         return $ Just r
    --     __ -> throw InvalidMessageTypeException

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

-- processEndPointRequests :: (HasService env m) => m ()
-- processEndPointRequests =
--     forever $ do
--         tcpE <- asks getEndPointEnv
--         let connSock = fst (tcpConn tcpE)
--         req <- liftIO $ atomically $ readTChan (reqQueue tcpE)
--         mp <- liftIO $ readTVarIO (msgMatch tcpE)
--         let nmp = M.insert (msgId (fst req)) (snd req) mp
--         liftIO $ atomically $ writeTVar (msgMatch tcpE) nmp
--         let body = serialise (fst req)
--         liftIO $ print (body)
--         let ma = LBS.length body
--         let xa = Prelude.fromIntegral (ma) :: Int16
--         T.sendLazy connSock (DB.encode (xa :: Int16))
--         T.sendLazy connSock (body)
--         return ()
-- decodeEndPointResponse :: M.Map Int (MVar EndPointMessage) -> LBS.ByteString -> IO ()
-- decodeEndPointResponse mp resp = do
--     let ipcReq = deserialise resp :: Maybe EndPointMessage
--     case ipcReq of
--         Just x -> do
--             printf "Decoded resp: %s\n" (show x)
--             let mid = msgId x
--             case (M.lookup mid mp) of
--                 Just k -> do
--                     liftIO $ putMVar k x
--                     return ()
--                 Nothing -> liftIO $ print ("Lookup failed.")
--         Nothing -> printf "Decode 'EndPointMessage' failed.\n" (show ipcReq)
-- handleResponse :: Socket -> TVar (M.Map Int (MVar EndPointMessage)) -> IO ()
-- handleResponse connSock mm =
--     forever $ do
--         lenBytes <- T.recv connSock 2
--         mp <- liftIO $ readTVarIO mm
--         case lenBytes of
--             Just l -> do
--                 let lenPrefix = runGet getWord16be l -- Char8.readInt l
--                 case lenPrefix of
--                     Right a -> do
--                         pl <- T.recv connSock (fromIntegral (toInteger a))
--                         case pl of
--                             Just y -> decodeEndPointResponse mp $ LBS.fromStrict y
--                             Nothing -> printf "Payload read error\n"
--                     Left _b -> printf "Length prefix corrupted.\n"
--             Nothing -> do
--                 printf "Connection closed.\n"
--                 liftIO $ threadDelay 15000000
--         return ()
--
-- processEndPointResponses :: (HasService env m) => m ()
-- processEndPointResponses = do
--     tcpE <- asks getEndPointEnv
--     let connSock = fst (tcpConn tcpE)
--     liftIO $ handleResponse connSock (msgMatch tcpE)
--     return ()
goGetResource :: (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => RPCCall -> m ()
goGetResource rpcCall = do
    let req = (request rpcCall)
    let ind = rpcIndex req
    let msg = rpcMessage req
    liftIO $ print ("fetchResource")
    resource <- fetchResource (RpcPayload AriviSecureRPC msg)
    case resource of
        Left e -> do
            liftIO $ print ("Exception: No peers available to issue RPC" ++ show e)
            let errMsg = Just "__EXCEPTION__NO_PEERS"
            liftIO $ (putMVar (response rpcCall) $ RPCIndMsg ind (RPCResponse 400 errMsg Nothing))
            return ()
        Right (RpcError _) -> liftIO $ print "Exception: RPC error"
        Right (RpcPayload _ respMsg) -> do
            liftIO $ print (respMsg)
            liftIO $ (putMVar (response rpcCall) $ RPCIndMsg ind (respMsg))
            return ()

loopRPC :: (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => (TChan RPCCall) -> m ()
loopRPC queue =
    forever $ do
        item <- liftIO $ atomically $ (readTChan queue)
        __ <- async (goGetResource item)
        return ()

loopPubSub :: (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => (TChan PubSubMsg) -> m ()
loopPubSub queue =
    forever $ do
        item <- liftIO $ atomically $ (readTChan queue)
        liftIO $ print ("read something..")
        case (item) of
            Subscribe' _t -> do
                topicVar <- asks topics
                liftIO $ atomically $ modifyTVar' topicVar (Set.insert (topic item))
            Publish' _t _m -> do
                liftIO $ print ("PUBLISH")
                Pub.publish (PubSubPayload ((topic item), (message item)))
            Notify' _t _m -> undefined
