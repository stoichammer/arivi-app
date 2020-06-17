{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module TLSServer where

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
import Data.Functor (($>))
import Data.IORef
import Data.Int
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Serialize
import Data.Text as T
import Data.X509.CertificateStore
import GHC.Generics
import qualified Network.Simple.TCP.TLS as TLS
import Network.Socket
import qualified Network.TLS as NTLS
import Service.Data
import Service.Env as NEnv
import Prelude as P
import Text.Printf
import NodeConfig as NC
import Service

data EncodingFormat
    = CBOR
    | JSON
    | DEFAULT

data TLSEndpointServiceHandler =
    TLSEndpointServiceHandler
        { connQueue :: TQueue EndPointConnection
        }

data EndPointConnection =
    EndPointConnection
        { requestQueue :: TQueue XDataReq
        , context :: MVar TLS.Context
        , encodingFormat :: IORef EncodingFormat
        }

newTLSEndpointServiceHandler :: IO TLSEndpointServiceHandler
newTLSEndpointServiceHandler = do
    conQ <- atomically $ newTQueue
    return $ TLSEndpointServiceHandler conQ

newEndPointConnection :: TLS.Context -> IO EndPointConnection
newEndPointConnection context = do
    reqQueue <- atomically $ newTQueue
    resLock <- newMVar context
    formatRef <- newIORef DEFAULT
    return $ EndPointConnection reqQueue resLock formatRef

handleRPCReqResp ::
       (HasService env m, MonadIO m)
    => MVar TLS.Context
    -> EncodingFormat
    -> Int
    -> Maybe String
    -> RPCMessage
    -> m ()
handleRPCReqResp sockMVar format mid version encReq = do
    nodeCnf <- getNodeConfig
    let net = bitcoinNetwork nodeCnf
    liftIO $ printf "handleRPCReqResp (%d, %s)\n" mid (show encReq)
    rpcResp <- goGetResource encReq net
    let body =
            case format of
                CBOR ->
                    serialise $
                    CBORRPCResponse (mid) (rsStatusCode rpcResp) (show <$> rsStatusMessage rpcResp) (rsBody rpcResp)
                JSON ->
                    case rsStatusMessage rpcResp of
                        Just err ->
                            A.encode
                                (JSONRPCErrorResponse
                                     mid
                                     (ErrorResponse (getJsonRPCErrorCode err) (show err) Nothing)
                                     (fromJust version))
                        Nothing -> A.encode (JSONRPCSuccessResponse (fromJust version) (rsBody rpcResp) mid)
    connSock <- liftIO $ takeMVar sockMVar
    let prefixbody = LBS.append (DB.encode (fromIntegral (LBS.length body) :: Int32)) body
    NTLS.sendData connSock prefixbody
    liftIO $ putMVar sockMVar connSock

handleNewConnectionRequest :: (HasService env m, MonadIO m) => TLSEndpointServiceHandler -> m ()
handleNewConnectionRequest handler = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleNewConnectionRequest\n"
        epConn <- liftIO $ atomically $ readTQueue $ connQueue handler
        async $ handleRequest epConn
        return ()

handleRequest :: (HasService env m, MonadIO m) => EndPointConnection -> m ()
handleRequest epConn = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        liftIO $ printf "handleRequest\n"
        xdReq <- liftIO $ atomically $ readTQueue $ requestQueue epConn
        case xdReq of
            XDataRPCReq mid met par version -> do
                liftIO $ printf "Decoded (%s)\n" (show met)
                let req = RPCRequest met par
                format <- liftIO $ readIORef (encodingFormat epConn)
                async (handleRPCReqResp (context epConn) format mid version req)
                return ()
            XDataRPCBadRequest -> do
                format <- liftIO $ readIORef (encodingFormat epConn)
                let body =
                        case format of
                            CBOR -> serialise $ CBORRPCResponse (-1) 400 (Just "Invalid request") Nothing
                            _ ->
                                A.encode $
                                JSONRPCErrorResponse
                                    (-1)
                                    (ErrorResponse (getJsonRPCErrorCode INVALID_REQUEST) (show INVALID_REQUEST) Nothing)
                                    "2.0"
                connSock <- liftIO $ takeMVar (context epConn)
                let prefixbody = LBS.append (DB.encode (fromIntegral (LBS.length body) :: Int32)) body
                NTLS.sendData connSock prefixbody
                liftIO $ putMVar (context epConn) connSock
            XCloseConnection -> do
                liftIO $ writeIORef continue False

enqueueRequest :: EndPointConnection -> LBS.ByteString -> IO ()
enqueueRequest epConn req = do
    format <- readIORef (encodingFormat epConn)
    xdReq <-
        case format of
            DEFAULT -> do
                case eitherDecode req of
                    Right JSONRPCRequest {..} ->
                        writeIORef (encodingFormat epConn) JSON $> XDataRPCReq id method params (Just jsonrpc)
                    Left err -> do
                        print $ "[Error] Decode failed: " <> show err
                        case deserialiseOrFail req of
                            Right CBORRPCRequest {..} ->
                                writeIORef (encodingFormat epConn) CBOR $> XDataRPCReq reqId method params Nothing
                            Left err -> do
                                print $ "[Error] Deserialize failed: " <> show err
                                return XDataRPCBadRequest
            JSON -> do
                case eitherDecode req of
                    Right JSONRPCRequest {..} -> return $ XDataRPCReq id method params (Just jsonrpc)
                    Left err -> do
                        print $ "[Error] Decode failed: " <> show err
                        return XDataRPCBadRequest
            CBOR -> do
                case deserialiseOrFail req of
                    Right CBORRPCRequest {..} -> return $ XDataRPCReq reqId method params Nothing
                    Left err -> do
                        print $ "[Error] Deserialize failed: " <> show err
                        return XDataRPCBadRequest
    atomically $ writeTQueue (requestQueue epConn) xdReq

handleConnection :: EndPointConnection -> TLS.Context -> IO ()
handleConnection epConn context = do
    continue <- liftIO $ newIORef True
    whileM_ (liftIO $ readIORef continue) $ do
        res <- try $ TLS.recv context
        case res of
            Right r ->
                case r of
                    Just l -> enqueueRequest epConn (LBS.fromStrict l)
                    Nothing -> putStrLn "Payload read error"
            Left (e :: IOException) -> do
                putStrLn "Connection closed."
                atomically $ writeTQueue (requestQueue epConn) XCloseConnection
                writeIORef continue False

startTLSEndpoint :: TLSEndpointServiceHandler -> String -> PortNumber -> [FilePath] -> IO ()
startTLSEndpoint handler listenIP listenPort [certFilePath, keyFilePath, certStoreFilePath] = do
    putStrLn $ "Starting TLS Endpoint"
    credentials <- NTLS.credentialLoadX509 certFilePath keyFilePath
    case credentials of
        Right cred
            -- cstore <- readCertificateStore mStoreFilePath
         -> do
            let settings = TLS.makeServerSettings cred Nothing
            TLS.serve settings (TLS.Host listenIP) (show listenPort) $ \(context, sockAddr) -> do
                putStrLn $ "client connection established : " ++ show sockAddr
                epConn <- newEndPointConnection context
                liftIO $ atomically $ writeTQueue (connQueue handler) epConn
                handleConnection epConn context
        Left err -> do
            putStrLn $ "Unable to read credentials from file"
            P.error "BadCredentialFile"
