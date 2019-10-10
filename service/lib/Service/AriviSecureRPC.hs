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
import  Shared_Types
import Control.Concurrent.MVar
import           Data.Int
import           Data.Map as M
import Data.Typeable

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

-- getAriviSecureRPC :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => (M.Map Int32 (MVar RPCCall)) -> m ()
-- getAriviSecureRPC mp = do
--     resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
--
--     liftIO $ print "here"
--     liftIO $ print resource


loopCall :: (HasP2PEnv env m ServiceResource ByteString String ByteString) => (M.Map Int32 (MVar RPCCall)) -> m ()
loopCall mp = do
    liftIO $ print "calling..."
    --getAriviSecureRPC
    resource <- fetchResource (RpcPayload AriviSecureRPC "<GET_Resource>")
    liftIO $ print resource
    liftIO $ print (typeOf mp)
    liftIO $ threadDelay 3000000

    loopCall mp
