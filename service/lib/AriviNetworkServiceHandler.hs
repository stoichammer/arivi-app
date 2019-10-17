
{-# LANGUAGE OverloadedStrings #-}

module AriviNetworkServiceHandler
    (
    AriviNetworkServiceHandler(..),
    newAriviNetworkServiceHandler,
    setupThriftDuplex,
    RPCCall(..)
    )
    where
import qualified AriviNetworkService
import AriviNetworkService_Iface
import Service_Types
import SharedService_Iface
import Shared_Types

--import Thrift
import Thrift.Protocol.Binary
--import Thrift.Transport
import Thrift.Server
--import Thrift.Transport
import Thrift.Transport.Handle

import Data.Int
import Data.String
import Data.Queue as Q
--import Data.Maybe
import Text.Printf
--import Control.Exception (throw)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import           Control.Concurrent.STM
--import Thrift.Transport.Empty
--import Control.Monad.IO.Class
import GHC.IO.Handle
--import Data.Text as T
--import Control.Monad.Logger (logDebug)


--import Data.Map ((!))
--import Data.Monoid
import           Network.Socket
import           Network
--import Control.Monad.IO.Class

--import           Service.AriviSecureRPC
--import Data.Typeable
--import AriviNetworkService_Client as CL


data RPCCall = RPCCall {
        request :: RPCReq ,
        response :: MVar RPCResp
}

data AriviNetworkServiceHandler = AriviNetworkServiceHandler {
                      ariviThriftLog :: MVar (M.Map Int32 SharedStruct)
                    , rpcQueue :: TChan RPCCall
                    , binProto :: BinaryProtocol Handle
                }

newAriviNetworkServiceHandler :: PortNumber -> IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler remotePort = do
  logg <- newMVar mempty
  rpcQ <- atomically $ newTChan
  --let localhost = "localhost" :: HostName
  transport  <- hOpen ( "localhost" :: HostName, PortNumber remotePort)
  return $ AriviNetworkServiceHandler logg rpcQ (BinaryProtocol transport)

instance SharedService_Iface AriviNetworkServiceHandler where

  getStruct self k = do
    myLog <- readMVar (ariviThriftLog self)
    return $ (myLog M.! k)

  getRPCReq self k = do
      x <- atomically $ peekTChan (rpcQueue self)
      let y = request x
      print (k)
      return y

  getRPCResp self k = do
      x <- atomically $ peekTChan (rpcQueue self)
      y <- readMVar (response x)
      print (k)
      return y
  -- getRPCCallItem self k = do
  --   queue <- readMVar (rpcQueue self)
  --   let key = (queue M.! k)
  --   val <- readMVar (key)
  --   return $ val

instance AriviNetworkService_Iface AriviNetworkServiceHandler where

    ping _ =
      return (True)

    -- args logid  & msg are passed from the remote side (thrift)
    sendRequest self mlogid mmsg = do
      printf "sendRequest(%d, %s)\n" logid (show msg)
      --printf "%s" show message_count ::Int32

      let req = payload msg
      --print (typeOf req)
      resp <- newEmptyMVar
      let rpcCall = RPCCall (RPCReq logid req) (resp)
      atomically $ writeTChan (rpcQueue self) rpcCall
      -- rpcEntry <- newMVar (RPCCall logid  req "_")
      -- modifyMVar_ (rpcQueue self) $ return .(M.insert logid rpcEntry)

      rpcResp <- (readMVar resp)
      let val = rPCResp_response rpcResp

      -- let val = case opcode msg of
      --             SET_NODE_CAPABILITY |
      --             GET_BLOCK_HEADERS -> "dummy-resp GET_BLOCK_HEADERS"
      --             GET_ESTIMATE_FEE -> "dummy-resp GET_ESTIMATE_FEE"
      --             SUBSCRIBE_NEW_HEADERS -> "dummy-resp SUBSCRIBE_NEW_HEADERS"
      --             GET_UNCONFIRMED_TX -> "dummy-resp GET_UNCONFIRMED_TX"
      --             GET_UTXOS -> "dummy-resp"
      --             SUBSCRIBE_SCRIPT_HASH -> "dummy-resp"
      --             UNSUBSCRIBE_SCRIPT_HASH -> "dummy-resp"
      --             BROADCAST_TX -> "dummy-resp"
      --             GET_RAW_TX_FROM_HASH -> "dummy-resp"
      --             GET_TX_MERKLE_PATH -> "dummy-resp"

      let logEntry = SharedStruct logid (fromString $ show $ val)
      modifyMVar_ (ariviThriftLog self) $ return .(M.insert logid logEntry)

      --let rpcEntry = newMVar (RPCCall logid req)
      --modifyMVar_ (rpcQueue self) $ return .(M.insert logid rpcEntry)

      return $! val

     where
       -- stupid dynamic languages f'ing it up
       --count = message_count
       --priority = message_priority
       --opcode = message_opcode
       payload = message_payload
       logid = mlogid
       msg = mmsg


setupThriftDuplex :: AriviNetworkServiceHandler -> PortNumber -> IO (AriviNetworkServiceHandler)
setupThriftDuplex handler listenPort =  do
  --handler <- newAriviNetworkServiceHandler
  printf "Starting thrift server..."
  _ <- runBasicServer handler AriviNetworkService.process listenPort
  --transport  <- hOpen ("localhost", remotePort)
  --let bp = BinaryProtocol transport
  --let client = (binProto, binProto)
  --putMVar (binProto handler) bp
  return ( handler)
