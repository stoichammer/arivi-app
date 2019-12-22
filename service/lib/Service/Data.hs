{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Data where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Exception
import Data.ByteString
import Data.Hashable
import Data.Int
import GHC.Generics
import Network.Xoken.Block
import Service.Types

-- import Control.Concurrent.MVar
-- import Control.Concurrent.STM
-- import qualified Data.ByteString as B
-- import Data.Functor.Identity
-- import qualified Data.Map.Strict as M
-- import Network.Socket hiding (send)
-- import Data.Time.Clock
-- import Data.Word
data XDataReq
    = XDataRPCReq
          { id :: Int
          , method :: String
          , params :: Maybe RPCReqParams
          }
    | XDataSubscribe
          { subscribeTopic :: String
          }
    | XDataPublish
          { publishTopic :: String
          , publishBody :: String
          }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data XDataResp
    = XDataRPCResp
          { rsStatusCode :: Int16
          , rsStatusMessage :: Maybe String
          , rsBody :: Maybe RPCResponseBody
          }
    | XDataNotify
          { notifyTopic :: String
          , notifyBody :: String
          }
    deriving (Show, Generic, Hashable, Eq, Serialise) --
    --
    -- data YMethod =
    --     YMethod
    --         { ss :: String
    --         , pp :: Int
    --         }
    --     deriving (Generic, Show, Eq, Serialise)
    --
    -- data XMethod =
    --     XMethod
    --         { s :: String
    --         , p :: XParams
    --         }
    --     deriving (Generic, Show, Eq, Serialise)
    --
    -- data XParams
    --     = XGetOne
    --           { ht :: Int
    --           }
    --     | XGetStr
    --           { st :: String
    --           }
    --     | XGetMany
    --           { hts :: [Int]
    --           }
    --     deriving (Generic, Show, Eq, Serialise)
    -- let x = serialise (XMethod "abc" $ XGetOne 100)
    -- let y = serialise (XMethod "xyz" $ XGetMany [1, 2, 3, 4, 5])
    -- print (x)
    -- print (y)
    -- let z = serialise (YMethod "ghi" $ 100)
    -- print (z)
