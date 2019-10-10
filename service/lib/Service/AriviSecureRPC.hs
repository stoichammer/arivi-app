{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Service.AriviSecureRPC
    ( module Service.AriviSecureRPC
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
import Control.Concurrent    (threadDelay)


type ServiceMsg = Lazy.ByteString

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

getAriviSecureRPC :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => m ()
getAriviSecureRPC = do
    resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
    liftIO $ print "here"
    liftIO $ print resource

loopCall :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => m ()
loopCall = do
    liftIO $ print "calling..."
    --getAriviSecureRPC
    resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
    liftIO $ print resource
    liftIO $ threadDelay 3000000
    loopCall
