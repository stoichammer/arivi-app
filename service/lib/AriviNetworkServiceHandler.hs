{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AriviNetworkServiceHandler
    ( AriviNetworkServiceHandler(..)
    , newAriviNetworkServiceHandler
    , setupEndPointServer
    , handleNewConnectionRequest
    , PubSubMsg(..)
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Fetch
import Arivi.P2P.Types
import Codec.Serialise
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Loops
import Data.Aeson as A
import Data.Binary as DB
import qualified Data.ByteString as B
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
        { connQueue :: TChan EndPointConnection
        }

data EndPointConnection =
    EndPointConnection
        { requestQueue :: TChan XDataReq
        , respWriteLock :: MVar Socket
        }

newAriviNetworkServiceHandler :: IO AriviNetworkServiceHandler
newAriviNetworkServiceHandler = do
    conQ <- atomically $ newTChan
    return $ AriviNetworkServiceHandler conQ

newEndPointConnection :: IO EndPointConnection
newEndPointConnection = do
    reqQueue <- atomically $ newTChan
    resLock <- newEmptyMVar
    return $ EndPointConnection reqQueue resLock

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

handleNewConnectionRequest ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => AriviNetworkServiceHandler -> m ()
handleNewConnectionRequest handler = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleNewConnectionRequest\n"
        epConn <- liftIO $ atomically $ readTChan $ connQueue handler
        async $ handleRequest epConn

handleRequest ::
       (HasP2PEnv env m ServiceResource ServiceTopic RPCMessage PubNotifyMessage) => EndPointConnection -> m ()
handleRequest handler = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleRequest\n"
        xdReq <- liftIO $ atomically $ readTChan $ requestQueue handler
        case xdReq of
            XDataRPCReq mid met par -> do
                liftIO $ printf "Decoded (%s)\n" (show met)
                let req = RPCRequest met par
                async (handleRPCReqResp (respWriteLock handler) mid req)
                return ()
            XDataSubscribe top -> do
                async (handlePubSubReqResp (respWriteLock handler) (Subscribe' top))
                return ()
            XDataPublish top body -> do
                async (handlePubSubReqResp (respWriteLock handler) (Publish' top $ PubNotifyMessage body))
                return ()
            XCloseConnection -> do
                liftIO $ writeIORef continue False

enqueueRequest :: EndPointConnection -> LBS.ByteString -> IO ()
enqueueRequest epConn req = do
    let xdReq = deserialise req :: XDataReq
    atomically $ writeTChan (requestQueue epConn) xdReq

handleConnection :: EndPointConnection -> Socket -> IO ()
handleConnection epConn connSock = do
    continue <- liftIO $ newIORef True
    putMVar (respWriteLock epConn) connSock
    whileM_ (liftIO $ readIORef continue) $ do
        res <- try $ recvAll connSock 2
        case res of
            Right l -> do
                let lenPrefix = runGet getWord16be l
                case lenPrefix of
                    Right a -> do
                        pl <- try $ recvAll connSock (fromIntegral (toInteger a))
                        case pl of
                            Right y -> do
                                enqueueRequest epConn (LBS.fromStrict y)
                                return ()
                            Left (e :: IOException) -> putStrLn "Payload read error"
                    Left _b -> putStrLn "Length prefix corrupted."
            Left (e :: IOException) -> do
                putStrLn "Connection closed."
                atomically $ writeTChan (requestQueue epConn) XCloseConnection
                writeIORef continue False

setupEndPointServer :: AriviNetworkServiceHandler -> String -> PortNumber -> IO ()
setupEndPointServer handler listenIP listenPort = do
    putStrLn $ "Starting Xoken Arch"
    _ <-
        serve (Host listenIP) (show listenPort) $ \(connSock, remoteAddr) -> do
            putStrLn $ "client connection established : " ++ show remoteAddr
            epConn <- newEndPointConnection
            atomically $ writeTChan (connQueue handler) epConn
            handleConnection epConn connSock
    return ()

-- Helper Functions
recvAll :: (MonadIO m) => Socket -> Int -> m B.ByteString
recvAll sock len = do
    if len > 0
        then do
            res <- liftIO $ try $ ST.recv sock len
            case res of
                Left (e :: IOException) -> throw e
                Right message ->
                    case message of
                        Nothing -> throw SocketReadException
                        Just mesg -> do
                            if B.length mesg == len
                                then return mesg
                                else if B.length mesg == 0
                                         then throw ZeroLengthSocketReadException
                                         else B.append mesg <$> recvAll sock (len - B.length mesg)
        else return (B.empty)
