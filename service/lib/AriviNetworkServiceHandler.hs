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
  -- , Subscribe1(..)
  -- , Notify1(..)
  -- , Publish(..)
  ) where

--import qualified AriviNetworkService
import AriviNetworkService_Iface

import Shared_Types

--import Codec.Serialise as S
import Data.Int
import Data.Queue as Q
import Data.String
import Data.Text.Lazy
import GHC.Generics
import Network.Simple.TCP

import Data.Serialize

--import Data.Maybe
import Text.Printf

--import Control.Exception (throw)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Data.Aeson as A
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

data RPCReq =
  RPCReq
    { rPCReq_key :: Int32
    , rPCReq_request :: Text
    }
  deriving (Show, Eq)

data RPCResp =
  RPCResp
    { rPCResp_key :: Int32
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
    { ariviThriftLog :: MVar (M.Map Int32 SharedStruct)
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

instance AriviNetworkService_Iface AriviNetworkServiceHandler where
  ping _ = return (True)
    -- args logid  & msg are passed from the remote side (thrift)
  sendRequest self mlogid jsonrequest = do
    printf "sendRequest(%d, %s)\n" logid (show req)
    --let req = payload msg
      --print (typeOf req)
    resp <- newEmptyMVar
    let rpcCall = RPCCall (RPCReq logid req) (resp)
    atomically $ writeTChan (rpcQueue self) rpcCall
    rpcResp <- (readMVar resp)
    let val = rPCResp_response rpcResp
    let logEntry = SharedStruct logid (fromString $ show $ val)
    modifyMVar_ (ariviThriftLog self) $ return . (M.insert logid logEntry)
    return $! val
       -- stupid dynamic languages f'ing it up
    where
      req = jsonrequest
      logid = mlogid
  subscribe self top = do
    printf "subscribe(%s)\n" top
    let sub = Subscribe1 (show top)
    atomically $ writeTChan (pubSubQueue self) sub
    return (top)
  publish self top msg = do
    printf "publish(%s, %s)\n" top msg
    let pub = Publish1 (show top) (show msg)
    atomically $ writeTChan (pubSubQueue self) pub
    return (top)
  notify self top msg = do
    printf "notify(%s, %s)\n" top msg
    let ntf = Notify1 (show top) (show msg)
    atomically $ writeTChan (pubSubQueue self) ntf
    return (top)

setupIPCServer :: AriviNetworkServiceHandler -> IO ()
setupIPCServer _handler = do
  printf "Starting TCP (IPC) server..."
  -- _ <- async (runBasicServer handler AriviNetworkService.process listenPort)
  _ <-
    serve (Host "127.0.0.1") "9090" $ \(connectionSocket, remoteAddr) -> do
      putStrLn $ "TCP connection established from " ++ show remoteAddr
      lenBytes <- recv connectionSocket 2
      case lenBytes of
        Just l -> do
          let lenPrefix = runGet getWord16be l -- Char8.readInt l
          case lenPrefix of
            Right a -> do
              payload <- recv connectionSocket (fromIntegral (toInteger a))
              case payload of
                Just y -> do
                  printf "received: %s\n" (Char8.unpack $ y)
                  let lz = (LBS.fromStrict y)
                  printf "lazy version: %s\n" (show lz)
                  let ipcReq = A.decode lz :: Maybe IPCRequest
                  case ipcReq of
                    Just x -> do
                      printf "Decoded (%s)\n" (show x)
                      printf
                        "params (%s)\n"
                        (show (M.lookup "encReq" (params x)))
                              --send connectionSocket (Char8.pack $ "Ok")
                    Nothing ->
                      printf "Decode 'IPCRequest' failed.\n" (show ipcReq)
                Nothing -> printf "Payload read error\n"
            Left _b -> printf "Length prefix corrupted."
        Nothing -> printf "Connection closed."
      return ()
  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.
  return ()
