{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Types where

import Codec.Serialise
import Control.Concurrent.MVar
import Control.Exception
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Hashable
import Data.Int
import GHC.Generics
import Network.Xoken.Block

data EndpointException
    = InvalidMessageTypeException
    | MessageParsingException
    | UnsupportedMethodException
    deriving (Show)

data PeerMessageException
    = SocketReadException
    | ZeroLengthSocketReadException
    deriving (Show)

instance Exception EndpointException

instance Exception PeerMessageException

--
-- INTERFACE START
--
data RPCMessage
    = RPCRequest
          { rqMethod :: String
          , rqParams :: Maybe RPCReqParams
          }
    | RPCResponse
          { rsStatusCode :: Int16
          , rsStatusMessage :: Maybe String
          , rsBody :: Maybe RPCResponseBody
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
          { gbBlockHash :: String
          }
    | GetBlocksByHashes
          { gbBlockHashes :: [String]
          }
    | GetTransactionByTxID
          { gtTxHash :: String
          }
    | GetTransactionsByTxIDs
          { gtTxHashes :: [String]
          }
    | GetOutputsByAddress
          { gaAddrOutputs :: String
          }
    | GetOutputsByAddresses
          { gasAddrOutputs :: [String]
          }
    | GetMerkleBranchByTxID
          { gmbMerkleBranch :: String
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
    | RespBlocksByHashes
          { blocks :: [BlockRecord]
          }
    | RespTransactionByTxID
          { tx :: TxRecord
          }
    | RespTransactionsByTxIDs
          { txs :: [TxRecord]
          }
    | RespOutputsByAddress
          { saddressOutputs :: [AddressOutputs]
          }
    | RespOutputsByAddresses
          { maddressOutputs :: [AddressOutputs]
          }
    | RespMerkleBranchByTxID
          { merkleBranch :: [MerkleBranchNode']
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

data BlockRecord =
    BlockRecord
        { rbHeight :: Int
        , rbHash :: String
        , rbHeader :: String
        }
    deriving (Generic, Show, Hashable, Eq, Serialise)

data TxRecord =
    TxRecord
        { txId :: String
        , txBlockInfo :: BlockInfo'
        , txSerialized :: C.ByteString
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data AddressOutputs =
    AddressOutputs
        { aoAddress :: String
        , aoOutput :: OutPoint'
        , aoBlockInfo :: BlockInfo'
        , aoIsBlockConfirmed :: Bool
        , aoIsOutputSpent :: Bool
        , aoIsTypeReceive :: Bool
        , aoOtherAddress :: String
        , aoPrevOutpoint :: OutPoint'
        , aoValue :: Int64
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data OutPoint' =
    OutPoint'
        { opTxHash :: String
        , opIndex :: Int
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data BlockInfo' =
    BlockInfo'
        { binfBlockHash :: String
        , binfTxIndex :: Int
        , binfBlockHeight :: Int
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data MerkleBranchNode' =
    MerkleBranchNode'
        { nodeValue :: String
        , isLeftNode :: Bool
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

data PubNotifyMessage =
    PubNotifyMessage
        { psBody :: String
        }
    deriving (Show, Generic, Eq, Serialise)

--
--
data PubSubMsg
    = Subscribe'
          { topic :: String
          }
    | Publish'
          { topic :: String
          , message :: PubNotifyMessage
          }
    | Notify'
          { topic :: String
          , message :: PubNotifyMessage
          }
    deriving (Show, Generic, Serialise)
