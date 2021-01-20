{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Data where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
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
--
data RPCMessage
    = RPCRequest
          { rqMethod :: String
          , rqParams :: Maybe RPCReqParams
          }
    | RPCResponse
          { rsStatusCode :: Int16
          , rsStatusMessage :: Maybe RPCErrors
          , rsBody :: Maybe RPCResponseBody
          }
    deriving (Show, Generic, Eq, Serialise)

data XRPCRequest
    = CBORRPCRequest
          { reqId :: Int
          , method :: String
          , params :: Maybe RPCReqParams
          }
    | JSONRPCRequest
          { method :: String
          , params :: Maybe RPCReqParams
          , jsonrpc :: String
          , id :: Int
          }
    deriving (Show, Generic, Eq, Serialise)

instance FromJSON XRPCRequest where
    parseJSON = genericParseJSON (defaultOptions {sumEncoding = UntaggedValue})

data XRPCResponse
    = CBORRPCResponse
          { matchId :: Int
          , statusCode :: Int16
          , statusMessage :: Maybe String
          , respBody :: Maybe RPCResponseBody
          }
    | JSONRPCSuccessResponse
          { jsonrpc :: String
          , result :: Maybe RPCResponseBody
          , id :: Int
          }
    | JSONRPCErrorResponse
          { id :: Int
          , error :: ErrorResponse
          , jsonrpc :: String
          }
    deriving (Show, Generic, Eq, Serialise)

instance ToJSON XRPCResponse where
    toJSON = genericToJSON (defaultOptions {sumEncoding = UntaggedValue})

data ErrorResponse =
    ErrorResponse
        { code :: Int
        , message :: String
        , _data :: Maybe String
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

instance ToJSON ErrorResponse where
    toJSON (ErrorResponse c m d) = object ["code" .= c, "message" .= m, "data" .= d]

data XDataReq
    = XDataRPCReq
          { reqId :: Int
          , method :: String
          , params :: Maybe RPCReqParams
          , version :: Maybe String
          }
    | XDataRPCBadRequest
    | XCloseConnection
    deriving (Show, Generic, Eq, Serialise)

data XDataResp =
    XDataRPCResp
        { matchId :: Int
        , statusCode :: Int16
        , statusMessage :: Maybe String
        , respBody :: Maybe RPCResponseBody
        }
    deriving (Show, Generic, Eq, Serialise, ToJSON)

data RPCErrors
    = INVALID_METHOD
    | PARSE_ERROR
    | INVALID_PARAMS
    | INTERNAL_ERROR
    | SERVER_ERROR
    | INVALID_REQUEST
    deriving (Generic, Hashable, Eq, Serialise)

instance Show RPCErrors where
    show e =
        case e of
            INVALID_METHOD -> "Error: Invalid method"
            PARSE_ERROR -> "Error: Parse error"
            INVALID_PARAMS -> "Error: Invalid params"
            INTERNAL_ERROR -> "Error: RPC error occurred"
            SERVER_ERROR -> "Error: Something went wrong"
            INVALID_REQUEST -> "Error: Invalid request"

-- can be replaced with Enum instance but in future other RPC methods might be handled then we might have to give different codes
getJsonRPCErrorCode :: RPCErrors -> Int
getJsonRPCErrorCode err =
    case err of
        SERVER_ERROR -> -32000
        INVALID_REQUEST -> -32600
        INVALID_METHOD -> -32601
        INVALID_PARAMS -> -32602
        INTERNAL_ERROR -> -32603
        PARSE_ERROR -> -32700
