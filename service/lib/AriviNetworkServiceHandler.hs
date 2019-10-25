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
  -- , Subscribe1(..)
  -- , Notify1(..)
  -- , Publish(..)
  ) where

--import qualified AriviNetworkService
-- import AriviNetworkService_Iface
import Shared_Types

--import Codec.Serialise as S
--import Data.Int
import Data.Queue as Q

--import Data.String
--import Data.Text.Lazy
import GHC.Generics
import Network.Simple.TCP

import Data.Serialize

import Control.Concurrent.Async.Lifted (async)
import Data.Text as T

import Data.Binary as DB
import Data.Int

--import Data.Maybe
import Text.Printf

--import Control.Exception (throw)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson as A

--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

--import Data.Text.Encoding
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
    { ariviThriftLog :: MVar (M.Map Int SharedStruct)
    , rpcQueue :: TChan RPCCall
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
  logg <- newMVar mempty
  rpcQ <- atomically $ newTChan
  psQ <- atomically $ newTChan
  --let localhost = "localhost" :: HostName
  --transport <- hOpen ("localhost" :: HostName, PortNumber remotePort)
  return $ AriviNetworkServiceHandler logg rpcQ psQ

-- instance AriviNetworkService_Iface AriviNetworkServiceHandler where
--   ping _ = return (True)
--     -- args logid  & msg are passed from the remote side (thrift)
--   subscribe self top = do
--     printf "subscribe(%s)\n" top
--     let sub = Subscribe1 (show top)
--     atomically $ writeTChan (pubSubQueue self) sub
--     return (top)
--   publish self top msg = do
--     printf "publish(%s, %s)\n" top msg
--     let pub = Publish1 (show top) (show msg)
--     atomically $ writeTChan (pubSubQueue self) pub
--     return (top)
--   notify self top msg = do
--     printf "notify(%s, %s)\n" top msg
--     let ntf = Notify1 (show top) (show msg)
--     atomically $ writeTChan (pubSubQueue self) ntf
--     return (top)
handleRPCReqResp :: MVar Socket -> TChan RPCCall -> Int -> String -> IO ()
handleRPCReqResp sockMVar rpcQ mid encReq = do
  printf "handleRPCReqResp(%d, %s)\n" mid (show encReq)
  resp <- newEmptyMVar
  let rpcCall = RPCCall (RPCReq mid (T.pack encReq)) (resp)
  atomically $ writeTChan (rpcQ) rpcCall
  rpcResp <- (readMVar resp)
  let val = unpack (rPCResp_response rpcResp)
  connSock <- takeMVar sockMVar
  let body = A.encode (IPCMessage mid "RPC_RESP" (M.singleton "encResp" val))
  --let body = encodeUtf8 serial
  let ma = LBS.length body
  let xa = Prelude.fromIntegral (ma) :: Int16
  sendLazy connSock (DB.encode (xa :: Int16))
  sendLazy connSock (body)
  putMVar sockMVar connSock
  return ()

handleConnection :: AriviNetworkServiceHandler -> Socket -> IO ()
handleConnection handler connSock =
  forever $ do
    lenBytes <- recv connSock 2
    case lenBytes of
      Just l -> do
        let lenPrefix = runGet getWord16be l -- Char8.readInt l
        case lenPrefix of
          Right a -> do
            payload <- recv connSock (fromIntegral (toInteger a))
            case payload of
              Just y -> do
                let lz = (LBS.fromStrict y)
                let ipcReq = A.decode lz :: Maybe IPCMessage
                case ipcReq of
                  Just x -> do
                    printf "Decoded (%s)\n" (show x)
                    sockMVar <- newMVar connSock
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
                  Nothing ->
                    printf "Decode 'IPCMessage' failed.\n" (show ipcReq)
              Nothing -> printf "Payload read error\n"
          Left _b -> printf "Length prefix corrupted.\n"
      Nothing -> printf "Connection closed.\n"

setupIPCServer :: AriviNetworkServiceHandler -> IO ()
setupIPCServer handler = do
  printf "Starting TCP (IPC) server..."
  _ <-
    serve (Host "127.0.0.1") "9090" $ \(connSock, remoteAddr) -> do
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      handleConnection handler connSock
  return ()
