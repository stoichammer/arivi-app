{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Types where

-- import Control.Concurrent.MVar
-- import Control.Concurrent.STM
-- import qualified Data.ByteString as B
-- import Data.Functor.Identity
import Data.Int

-- import qualified Data.Map.Strict as M
--
-- import Data.Time.Clock
-- import Data.Word
import Codec.Serialise
import Data.ByteString
import Data.Hashable
import GHC.Generics

-- import Network.Socket hiding (send)
import Network.Xoken.Block

-- import Network.Xoken.Constants
-- import Network.Xoken.Crypto.Hash
-- import Network.Xoken.Network
-- import Network.Xoken.Transaction
-- import System.Random
-- import Text.Read
data RPCMessage
    = RPCRequest
          { rqMethod :: String
          , rqParams :: Maybe RPCReqParams
          }
    | RPCResponse
          { rsStatusCode :: Int16
          , rsStatusMessage :: Maybe String
          , rsBody :: RPCResponseBody
          }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data RPCReqParams
    = GetBlockByHeight
          { gbHeight :: Int
          }
    | GetBlocksByHeight
          { gbHeights :: [Int]
          }
    | GetBlockByHash
          { gbBlockHash :: BlockHash
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

data RPCResponseBody
    = RespBlockByHeight
          { block :: BlockRecord
          }
    | RespBlocksByHeight
          { blocks :: [BlockRecord]
          }
    | RespBlockByHash
          { block :: BlockRecord
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

data PubNotifyMessage =
    PubNotifyMessage
        { psBody :: ByteString
        }
    deriving (Show, Generic, Eq, Serialise)

--
data BlockRecord =
    BlockRecord
        { rbHeight :: Int
        , rbHash :: BlockHash
        , rbHeader :: BlockHeader
        }
    deriving (Generic, Show, Hashable, Eq, Serialise)
