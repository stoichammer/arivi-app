{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Types where

import Codec.Compression.GZip as GZ
import Codec.Serialise
import Control.Applicative
import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
import Data.ByteString
import Data.ByteString.Base64 as B64
import Data.ByteString.Base64.Lazy as B64L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Hashable
import Data.Int
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.Xoken.Block
import Network.Xoken.Crypto.Hash
import UtxoPool

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
data RPCReqParams
    = AddXPubKey
          { xpubKey :: ByteString
          , addressCount :: Int
          , allegoryHash :: String
          }
    | GetNextAddress
          { allegoryHash :: String
          }
    | PSAllpayTransaction
          { inputs :: [(OutPoint', Int64)]
          , recipient :: String
          , amount :: Int64
          , change :: String
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

instance FromJSON RPCReqParams where
    parseJSON (Object o) =
        (AddXPubKey <$> (T.encodeUtf8 <$> o .: "xpubKey") <*> o .: "addressCount" <*> o .: "allegoryHash") <|>
        (GetNextAddress <$> o .: "allegoryHash") <|>
        (PSAllpayTransaction <$> o .: "inputs" <*> o .: "recipient" <*> o .: "amount" <*> o .: "change")

data RPCResponseBody
    = RespXPubKey
          { rxpb :: Bool
          }
    | RespGetNextAddress
          { address :: String
          , merklePath :: PartialMerkleTree
          , ppo :: ProxyProviderUtxo
          }
    | RespPSAllpayTransaction
          { serialisedTx :: ByteString
          , addressProof :: PartialMerkleTree
          , utxoProof :: PartialMerkleTree
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

instance ToJSON RPCResponseBody where
    toJSON (RespXPubKey rxpb) = object ["rxpb" .= rxpb]
    toJSON (RespGetNextAddress a mp ppo) = object ["address" .= a, "merklePath" .= mp, "ppOutpoint" .= ppo]
    toJSON (RespPSAllpayTransaction stx ap up) = object ["tx" .= stx, "addressProof" .= ap, "utxoProof" .= up]

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

instance FromJSON OutPoint' where
    parseJSON (Object o) = (OutPoint' <$> o .: "txid" <*> o .: "index")

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
