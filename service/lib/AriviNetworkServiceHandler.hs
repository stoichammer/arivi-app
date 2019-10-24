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
  , IPCRequest(..)
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
import Data.Text.Lazy
import GHC.Generics
import Network.Simple.TCP

import Data.Serialize

--import Data.Maybe
import Text.Printf

import Control.Concurrent.Async.Lifted (async)

--import Control.Exception (throw)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Aeson as A
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

data RPCReq =
  RPCReq
    { rPCReq_key :: Int
    , rPCReq_request :: Text
    }
  deriving (Show, Eq)

data RPCResp =
  RPCResp
    { rPCResp_key :: Int
    , rPCResp_response :: Text
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

data IPCRequest =
  IPCRequest
    { msgid :: Int
    , mtype :: String
    , params :: M.Map String String
    }
  deriving (Show, Generic)

instance ToJSON IPCRequest

instance FromJSON IPCRequest

--instance Serialise IPCRequest
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
--   sendRequest self mlogid jsonrequest = do
--     printf "sendRequest(%d, %s)\n" logid (show req)
--     --let req = payload msg
--       --print (typeOf req)
--     resp <- newEmptyMVar
--     let rpcCall = RPCCall (RPCReq logid req) (resp)
--     atomically $ writeTChan (rpcQueue self) rpcCall
--     rpcResp <- (readMVar resp)
--     let val = rPCResp_response rpcResp
--     let logEntry = SharedStruct logid (fromString $ show $ val)
--     modifyMVar_ (ariviThriftLog self) $ return . (M.insert logid logEntry)
--     return $! val
--        -- stupid dynamic languages f'ing it up
--     where
--       req = jsonrequest
--       logid = mlogid
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
sendRequest :: MVar Socket -> TChan RPCCall -> Int -> String -> IO ()
sendRequest sockMVar rpcQ mid encReq = do
  printf "sendRequest(%d, %s)\n" mid (show encReq)
  resp <- newEmptyMVar
  let rpcCall = RPCCall (RPCReq mid (pack encReq)) (resp)
  atomically $ writeTChan (rpcQ) rpcCall
  rpcResp <- (readMVar resp)
  let val = rPCResp_response rpcResp
  connSock <- takeMVar sockMVar
  send connSock (Char8.pack $ (show val))
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
                let ipcReq = A.decode lz :: Maybe IPCRequest
                case ipcReq of
                  Just x -> do
                    printf "Decoded (%s)\n" (show x)
                    sockMVar <- newMVar connSock
                    case (M.lookup "encReq" (params x)) of
                      Just enc -> do
                        _ <-
                          async
                            (sendRequest
                               (sockMVar)
                               (rpcQueue handler)
                               (msgid x)
                               (enc))
                        return ()
                      Nothing -> printf "Invalid payload.\n"
                  Nothing ->
                    printf "Decode 'IPCRequest' failed.\n" (show ipcReq)
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
