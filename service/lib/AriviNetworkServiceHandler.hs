{-# LANGUAGE OverloadedStrings #-}

module AriviNetworkServiceHandler
  ( AriviNetworkServiceHandler(..)
  , newAriviNetworkServiceHandler
  , setupThriftDuplex
  , RPCCall(..)
  , RPCReq(..)
  , RPCResp(..)
  , PubSub(..)
  -- , Subscribe1(..)
  -- , Notify1(..)
  -- , Publish(..)
  )
where

import qualified AriviNetworkService
import           AriviNetworkService_Iface
import           Control.Concurrent.Async.Lifted
                                                ( async )
--import           Service_Types                  ( )
--import           SharedService_Iface
import           Shared_Types

--import Thrift
import           Thrift.Protocol.Binary

--import Thrift.Transport
import           Thrift.Server

--import Thrift.Transport
import           Thrift.Transport.Handle

import           Data.Int
import           Data.Queue                    as Q
import           Data.String
import           Data.Text.Lazy
--import Data.Maybe
import           Text.Printf

--import Control.Exception (throw)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import qualified Data.Map.Strict               as M

--import Thrift.Transport.Empty
--import Control.Monad.IO.Class
import           GHC.IO.Handle

--import Data.Text as T
--import Control.Monad.Logger (logDebug)
--import Data.Map ((!))
--import Data.Monoid
import           Network

--import           Network.Socket
--import Control.Monad.IO.Class
--import           Service.AriviSecureRPC
--import Data.Typeable
--import AriviNetworkService_Client as CL

data RPCReq = RPCReq  { rPCReq_key :: Int32
  , rPCReq_request :: Text
  } deriving (Show,Eq)

data RPCResp = RPCResp  { rPCResp_key :: Int32
  , rPCResp_response :: Text
  } deriving (Show,Eq)

data RPCCall =
  RPCCall
    { request :: RPCReq
    , response :: MVar RPCResp
    }


data PubSub = Subscribe1 {topic :: String}
            | Publish1 { topic :: String
                    , message :: String}
            | Notify1 { topic :: String
                    , message :: String}

data AriviNetworkServiceHandler =
  AriviNetworkServiceHandler
    { ariviThriftLog :: MVar (M.Map Int32 SharedStruct)
    , rpcQueue :: TChan RPCCall
    , pubSubQueue :: TChan PubSub
    , binProto :: BinaryProtocol Handle
    }

newAriviNetworkServiceHandler :: PortNumber -> IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler remotePort = do
  logg      <- newMVar mempty
  rpcQ      <- atomically $ newTChan
  psQ       <- atomically $ newTChan
  --let localhost = "localhost" :: HostName
  transport <- hOpen ("localhost" :: HostName, PortNumber remotePort)
  return $ AriviNetworkServiceHandler logg rpcQ psQ (BinaryProtocol transport)

-- instance SharedService_Iface AriviNetworkServiceHandler where
--   getStruct self k = do
--     myLog <- readMVar (ariviThriftLog self)
--     return $ (myLog M.! k)
--   getRPCReq self k = do
--     x <- atomically $ peekTChan (rpcQueue self)
--     let y = request x
--     print (k)
--     return y
--   getRPCResp self k = do
--     x <- atomically $ peekTChan (rpcQueue self)
--     y <- readMVar (response x)
--     print (k)
--     return y
  -- getRPCCallItem self k = do
  --   queue <- readMVar (rpcQueue self)
  --   let key = (queue M.! k)
  --   val <- readMVar (key)
  --   return $ val

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

  publish self top msg = do
    printf "publish(%s, %s)\n" top msg

    let pub = Publish1 (show top) (show msg)
    atomically $ writeTChan (pubSubQueue self) pub

  notify self top msg = do
    printf "notify(%s, %s)\n" top msg

    let ntf = Notify1 (show top) (show msg)
    atomically $ writeTChan (pubSubQueue self) ntf

setupThriftDuplex :: PortNumber -> PortNumber -> IO (AriviNetworkServiceHandler)
setupThriftDuplex listenPort remotePort = do
  handler <- newAriviNetworkServiceHandler remotePort
  printf "Starting thrift server..."
  _ <- async (runBasicServer handler AriviNetworkService.process listenPort)
  --transport  <- hOpen ("localhost", remotePort)
  --let bp = BinaryProtocol transport
  --let client = (binProto, binProto)
  --putMVar (binProto handler) bp
  return (handler)
