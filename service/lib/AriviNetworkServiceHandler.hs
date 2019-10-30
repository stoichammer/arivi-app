{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module AriviNetworkServiceHandler
    ( AriviNetworkServiceHandler(..)
    , newAriviNetworkServiceHandler
    , setupIPCServer
    , RPCCall(..)
    , RPCReq(..)
    , RPCResp(..)
    , PubSubMsg(..)
    , IPCMessage(..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson as A
import Data.Binary as DB
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.Map.Strict as M
import Data.Serialize
import Data.Text as T
import GHC.Generics
import Network.Simple.TCP as ST
import Network.Socket
import Text.Printf

data RPCReq =
    RPCReq
        { rPCReq_key :: Int
        , rPCReq_request :: T.Text
        }
    deriving (Show, Eq)

data RPCResp =
    RPCResp
        { rPCResp_key :: Int
        , rPCResp_response :: T.Text
        }
    deriving (Show, Eq)

data RPCCall =
    RPCCall
        { request :: RPCReq
        , response :: MVar RPCResp
        }

data PubSubMsg
    = Subscribe1
          { topic :: String
          }
    | Publish1
          { topic :: String
          , message :: String
          }
    | Notify1
          { topic :: String
          , message :: String
          }

data AriviNetworkServiceHandler =
    AriviNetworkServiceHandler
        { rpcQueue :: TChan RPCCall
        , pubSubQueue :: TChan PubSubMsg
        }

data IPCMessage =
    IPCMessage
        { msgid :: Int
        , mtype :: String
        , params :: M.Map String String
        }
    deriving (Show, Generic)

instance ToJSON IPCMessage

instance FromJSON IPCMessage

--instance Serialise IPCMessage
newAriviNetworkServiceHandler :: IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler = do
    rpcQ <- atomically $ newTChan
    psQ <- atomically $ newTChan
    return $ AriviNetworkServiceHandler rpcQ psQ

handleRPCReqResp :: MVar Socket -> TChan RPCCall -> Int -> String -> IO ()
handleRPCReqResp sockMVar rpcQ mid encReq = do
    printf "handleRPCReqResp(%d, %s)\n" mid (show encReq)
    resp <- newEmptyMVar
    let rpcCall = RPCCall (RPCReq mid (T.pack encReq)) (resp)
    atomically $ writeTChan (rpcQ) rpcCall
    rpcResp <- (readMVar resp)
    let val = unpack (rPCResp_response rpcResp)
    let body = A.encode (IPCMessage mid "RPC_RESP" (M.singleton "encResp" val))
    let ma = LBS.length body
    let xa = Prelude.fromIntegral (ma) :: Int16
    connSock <- takeMVar sockMVar
    sendLazy connSock (DB.encode (xa :: Int16))
    sendLazy connSock (body)
    putMVar sockMVar connSock
    return ()

handleSubscribeReqResp :: MVar Socket -> TChan PubSubMsg -> Int -> String -> IO ()
handleSubscribeReqResp sockMVar psQ mid subject = do
    printf "handleSubscribeReqResp(%d, %s)\n" mid (subject)
    let sub = Subscribe1 (subject)
    atomically $ writeTChan (psQ) sub
    let body = A.encode (IPCMessage mid "SUB_RESP" (M.singleton "status" "ACK"))
    let ma = LBS.length body
    let xa = Prelude.fromIntegral (ma) :: Int16
    connSock <- takeMVar sockMVar
    sendLazy connSock (DB.encode (xa :: Int16))
    sendLazy connSock (body)
    putMVar sockMVar connSock
    return ()

handlePublishReqResp :: MVar Socket -> TChan PubSubMsg -> Int -> String -> String -> IO ()
handlePublishReqResp sockMVar psQ mid subject body = do
    printf "handlePublishReqResp(%d, %s, %s)\n" mid subject body
    let pub = Publish1 (subject) (body)
    atomically $ writeTChan (psQ) pub
    let body1 = A.encode (IPCMessage mid "PUB_RESP" (M.singleton "status" "ACK"))
    let ma = LBS.length body1
    let xa = Prelude.fromIntegral (ma) :: Int16
    connSock <- takeMVar sockMVar
    sendLazy connSock (DB.encode (xa :: Int16))
    sendLazy connSock (body1)
    putMVar sockMVar connSock
    return ()

handleConnection :: AriviNetworkServiceHandler -> Socket -> IO ()
handleConnection handler connSock =
    forever $ do
        lenBytes <- ST.recv connSock 2
        case lenBytes of
            Just l -> do
                let lenPrefix = runGet getWord16be l -- Char8.readInt l
                case lenPrefix of
                    Right a -> do
                        payload <- ST.recv connSock (fromIntegral (toInteger a))
                        case payload of
                            Just y -> do
                                let lz = (LBS.fromStrict y)
                                let ipcReq = A.decode lz :: Maybe IPCMessage
                                case ipcReq of
                                    Just x -> do
                                        printf "Decoded (%s)\n" (show x)
                                        sockMVar <- newMVar connSock
                                        case (mtype x) of
                                            "RPC_REQ" -> do
                                                case (M.lookup "encReq" (params x)) of
                                                    Just enc -> do
                                                        _ <-
                                                            async
                                                                (handleRPCReqResp
                                                                     (sockMVar)
                                                                     (rpcQueue handler)
                                                                     (msgid x)
                                                                     (enc))
                                                        return ()
                                                    Nothing -> printf "Invalid payload.\n"
                                            "SUB_REQ" -> do
                                                case (M.lookup "subject" (params x)) of
                                                    Just su -> do
                                                        _ <-
                                                            async
                                                                (handleSubscribeReqResp
                                                                     sockMVar
                                                                     (pubSubQueue handler)
                                                                     (msgid x)
                                                                     su)
                                                        return ()
                                                    Nothing -> printf "Invalid payload.\n"
                                            "PUB_REQ" -> do
                                                case (M.lookup "subject" (params x)) of
                                                    Just su -> do
                                                        case (M.lookup "body" (params x)) of
                                                            Just bdy -> do
                                                                _ <-
                                                                    async
                                                                        (handlePublishReqResp
                                                                             sockMVar
                                                                             (pubSubQueue handler)
                                                                             (msgid x)
                                                                             su
                                                                             bdy)
                                                                return ()
                                                            Nothing -> printf "Invalid payload.\n"
                                                    Nothing -> printf "Invalid payload.\n"
                                            __ -> printf "Invalid message type.\n"
                                    Nothing -> printf "Decode 'IPCMessage' failed.\n" (show ipcReq)
                            Nothing -> printf "Payload read error\n"
                    Left _b -> printf "Length prefix corrupted.\n"
            Nothing -> do
                printf "Connection closed.\n"
                liftIO $ threadDelay 15000000

setupIPCServer :: AriviNetworkServiceHandler -> PortNumber -> IO ()
setupIPCServer handler listenPort = do
    printf "Starting TCP (IPC) server..."
    _ <-
        serve (Host "127.0.0.1") (show listenPort) $ \(connSock, remoteAddr) -> do
            putStrLn $ "TCP connection established from " ++ show remoteAddr
            handleConnection handler connSock
    return ()
