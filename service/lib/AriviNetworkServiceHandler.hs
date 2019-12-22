{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AriviNetworkServiceHandler
    ( AriviNetworkServiceHandler(..)
    , newAriviNetworkServiceHandler
    , setupEndPointServer
    , RPCCall(..)
    , PubSubMsg(..)
    , EndPointMessage(..)
    ) where

import Codec.Serialise
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
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
import Service.Types
import Text.Printf

data AriviNetworkServiceHandler =
    AriviNetworkServiceHandler
        { rpcQueue :: TChan RPCCall
        , pubSubQueue :: TChan PubSubMsg
        }

newAriviNetworkServiceHandler :: IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler = do
    rpcQ <- atomically $ newTChan
    psQ <- atomically $ newTChan
    return $ AriviNetworkServiceHandler rpcQ psQ

handleRPCReqResp :: MVar Socket -> TChan RPCCall -> Int -> RPCMessage -> IO ()
handleRPCReqResp sockMVar rpcQ mid encReq = do
    printf "handleRPCReqResp(%d, %s)\n" mid (show encReq)
    resp <- newEmptyMVar
    let rpcCall = RPCCall (RPCIndMsg mid encReq) (resp)
    atomically $ writeTChan (rpcQ) rpcCall
    rpcResp <- (readMVar resp)
    let body = serialise (EndPointMessage mid (RPC $ rpcMessage rpcResp))
    let ma = LBS.length body
    let xa = Prelude.fromIntegral (ma) :: Int16
    connSock <- takeMVar sockMVar
    sendLazy connSock (DB.encode (xa :: Int16))
    sendLazy connSock (body)
    putMVar sockMVar connSock
    return ()

handlePubSubReqResp :: MVar Socket -> TChan PubSubMsg -> PubSubMsg -> IO ()
handlePubSubReqResp sockMVar psQ sub = do
    print ("handleSubscribeReqResp " ++ show sub)
    atomically $ writeTChan (psQ) sub
    -- let body = serialise (EndPointMessage mid (PSN sub))
    -- let ma = LBS.length body
    -- let xa = Prelude.fromIntegral (ma) :: Int16
    -- connSock <- takeMVar sockMVar
    -- sendLazy connSock (DB.encode (xa :: Int16))
    -- sendLazy connSock (body)
    -- putMVar sockMVar connSock
    return ()

decodeRequest :: AriviNetworkServiceHandler -> MVar Socket -> LBS.ByteString -> IO ()
decodeRequest handler sockMVar req = do
    let xdReq = deserialise req :: XDataReq
    case xdReq of
        XDataRPCReq mid met par -> do
            printf "Decoded (%s)\n" (show met)
            let req = RPCRequest met par
            _ <- async (handleRPCReqResp (sockMVar) (rpcQueue handler) (mid) req)
            return ()
        XDataSubscribe top -> do
            _ <- async (handlePubSubReqResp sockMVar (pubSubQueue handler) (Subscribe' top))
            return ()
        XDataPublish top body -> do
            _ <- async (handlePubSubReqResp sockMVar (pubSubQueue handler) (Publish' top $ PubNotifyMessage body))
            return ()

handleConnection :: AriviNetworkServiceHandler -> Socket -> IO ()
handleConnection handler connSock = do
    continue <- liftIO $ newIORef True
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
                                sockMVar <- newMVar connSock
                                decodeRequest handler sockMVar (LBS.fromStrict y)
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
