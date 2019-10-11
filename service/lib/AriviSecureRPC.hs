{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module AriviSecureRPC
    (
        ServiceResource(..) ,
        handler,
        loopCall
    ) where

import Arivi.P2P.P2PEnv
import Arivi.P2P.RPC.Functions
import Arivi.P2P.RPC.Types
import Arivi.P2P.Types

import GHC.Generics
import Codec.Serialise
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy
import Data.Hashable
-- import Control.Concurrent    (threadDelay)
import  Shared_Types
--import Control.Concurrent.MVar
-- import           Data.Int
--import           Data.Map.Strict as M
import Data.Typeable
import           Control.Concurrent.STM
import           Control.Concurrent.Async.Lifted (async)
import AriviNetworkServiceHandler

--type ServiceMsg = Lazy.ByteString

data ServiceResource = AriviSecureRPC deriving (Eq, Ord, Show, Generic)

instance Serialise ServiceResource
instance Hashable ServiceResource



handler :: ResourceHandler ServiceResource String
handler = ResourceHandler (\(RpcPayload resource serviceMsg) -> RpcPayload resource (serviceMsg ++ " < Resource_resp>"))

-- registerAriviSecureRPC :: (HasP2PEnv env m ServiceResource String String String) => m ()
-- registerAriviSecureRPC =
--     registerResource AriviSecureRPC handler Archived >>
--     liftIO (threadDelay 5000000) >>
--     updatePeerInResourceMap AriviSecureRPC

-- getAriviSecureRPC :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => (M.Map Int32 (MVar RPCCall)) -> m ()
-- getAriviSecureRPC mp = do
--     resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
--
--     liftIO $ print "here"
--     liftIO $ print resource

theMessage :: Either a (RpcPayload b String) -> Maybe String;
theMessage (Right (RpcPayload _ str)) = Just str;
theMessage _ = Nothing

goGetResource :: (HasP2PEnv env m ServiceResource ByteString String ByteString)
        => RPCCall -> m ()
goGetResource rpcCall = do
     let req = (request rpcCall)
     --let ind = rPCReq_key req
     let msg = show (rPCReq_request req)
     liftIO $ print (msg)
     resource <- fetchResource (RpcPayload AriviSecureRPC msg)
     liftIO $ print (typeOf resource)
     liftIO $ print (theMessage resource)
     case resource of
         Left _   -> liftIO $ print "Exception AriviP2PException"
         Right _rr -> do
                    liftIO $ print resource
                    --respMsg <- msg rr
                    --liftIO $ print respMsg
                    --putMVar (response rpcCall) (RPCResp ind respMsg)

     return ()



loopCall :: (HasP2PEnv env m ServiceResource ByteString String ByteString)
            => ( TChan RPCCall) -> m ()
loopCall queue = do

        item <- liftIO $ atomically $ (readTChan queue)
        liftIO $ print (typeOf item)
        let req =  (request item)
        _ <- async (goGetResource item)
        liftIO $ print (req )
        loopCall queue
        return ()
        -- case (lookupMax mp) of
        --     Just c ->   do
        --                     y <- liftIO (readMVar (snd c))
        --
        --                     liftIO $ print "looping..."
        --                     --getAriviSecureRPC
        --
        --                     --resource <- fetchResource (RpcPayload AriviSecureRPC (show (rPCCall_request (y))))
        --
        --                     loopCall mp
        --                     return()
        --     Nothing ->  return()
