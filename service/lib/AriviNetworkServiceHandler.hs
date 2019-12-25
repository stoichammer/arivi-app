{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module AriviNetworkServiceHandler
    ( AriviNetworkServiceHandler(..)
    , newAriviNetworkServiceHandler
    , setupEndPointServer
    , handleRequest
    , RPCCall(..)
    , PubSubMsg(..)
    , EndPointMessage(..)
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Aeson as A
import Data.Binary as DB
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as M
import Data.Serialize
import Data.Text as T
import GHC.Generics
import Network.Simple.TCP as ST
import Network.Socket
import Service.Data
import Service.Env
import Service.Types
import Text.Printf

data AriviNetworkServiceHandler =
    AriviNetworkServiceHandler
        { requestQueue :: TChan XDataReq
        , respWriteLock :: MVar Socket
        }

newAriviNetworkServiceHandler :: IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler = do
    reqQueue <- atomically $ newTChan
    resLock <- newEmptyMVar
    return $ AriviNetworkServiceHandler reqQueue resLock

goGetResource ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => RPCMessage -> m (RPCMessage)
goGetResource msg = do
    liftIO $ print ("fetchResource")
    resource <- fetchResource (RpcPayload AriviSecureRPC msg)
    case resource of
        Left e -> do
            liftIO $ print ("Exception: No peers available to issue RPC" ++ show e)
            return (RPCResponse 400 (Just "No connected peers") Nothing)
        Right (RpcError _) -> return (RPCResponse 500 (Just "Unknown RPC error") Nothing)
        Right (RpcPayload _ respMsg) -> do
            liftIO $ print (respMsg)
            return (respMsg)

handleRPCReqResp ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage)
    => MVar Socket
    -> Int
    -> RPCMessage
    -> m ()
handleRPCReqResp sockMVar mid encReq = do
    liftIO $ printf "handleRPCReqResp(%d, %s)\n" mid (show encReq)
    rpcResp <- goGetResource encReq
    let body = serialise $ XDataRPCResp (mid) (rsStatusCode rpcResp) (rsStatusMessage rpcResp) (rsBody rpcResp)
    connSock <- liftIO $ takeMVar sockMVar
    sendLazy connSock $ DB.encode (Prelude.fromIntegral (LBS.length body) :: Int16)
    sendLazy connSock body
    liftIO $ putMVar sockMVar connSock
    return ()

handlePubSubReqResp ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => MVar Socket -> PubSubMsg -> m ()
handlePubSubReqResp sockMVar sub = do
    liftIO $ print ("handleSubscribeReqResp " ++ show sub)
    -- INSERT
    return ()

handleRequest ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => AriviNetworkServiceHandler -> m ()
handleRequest handler = do
    xdReq <- liftIO $ atomically $ readTChan $ requestQueue handler
    case xdReq of
        XDataRPCReq mid met par -> do
            liftIO $ printf "Decoded (%s)\n" (show met)
            let req = RPCRequest met par
            _ <- async (handleRPCReqResp (respWriteLock handler) mid req)
            return ()
        XDataSubscribe top -> do
            _ <- async (handlePubSubReqResp (respWriteLock handler) (Subscribe' top))
            return ()
        XDataPublish top body -> do
            _ <- async (handlePubSubReqResp (respWriteLock handler) (Publish' top $ PubNotifyMessage body))
            return ()

enqueueRequest :: AriviNetworkServiceHandler -> LBS.ByteString -> IO ()
enqueueRequest handler req = do
    let xdReq = deserialise req :: XDataReq
    atomically $ writeTChan (requestQueue handler) xdReq

handleConnection :: AriviNetworkServiceHandler -> Socket -> IO ()
handleConnection handler connSock = do
    continue <- liftIO $ newIORef True
    putMVar (respWriteLock handler) connSock
    whileM_ (liftIO $ readIORef continue) $ do
        lenBytes <- ST.recv connSock 2
        case lenBytes of
            Just l -> do
                let lenPrefix = runGet getWord16be l
                case lenPrefix of
                    Right a -> do
                        pl <- ST.recv connSock (fromIntegral (toInteger a))
                        case pl of
                            Just y -> do
                                enqueueRequest handler (LBS.fromStrict y)
                                return ()
                            Nothing -> printf "Payload read error\n"
                    Left _b -> printf "Length prefix corrupted.\n"
            Nothing -> do
                printf "Connection closed.\n"
                writeIORef continue False

setupEndPointServer :: AriviNetworkServiceHandler -> PortNumber -> IO ()
setupEndPointServer handler listenPort = do
    printf "Starting EndPoint listener..\n"
    _ <-
        serve (Host "127.0.0.1") (show listenPort) $ \(connSock, remoteAddr) -> do
            putStrLn $ "EndPoint connection established from " ++ show remoteAddr
            handleConnection handler connSock
    return ()
