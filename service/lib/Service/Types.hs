{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import Data.Text as DT
import qualified Data.Text.Encoding as T
import Data.Word
import GHC.Generics
import Network.Xoken.Block
import Network.Xoken.Crypto.Hash
import Network.Xoken.Transaction

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
    | PSAllpayTransaction
          { inputs :: [(OutPoint', Int64)]
          , recipient :: String
          , amount :: Int64
          , change :: String
          }
    | Register
          { rName :: [Int]
          , rXpk :: ByteString
          , rNutxo :: (OutPoint', Int64)
          , rRetAddr :: String
          , rCount :: Int
          }
    | GetCoins
          { userAddress :: String
          }
    deriving (Generic, Show, Eq, Serialise)

instance FromJSON RPCReqParams where
    parseJSON (Object o) =
        (AddXPubKey <$> (T.encodeUtf8 <$> o .: "xpubKey") <*> o .: "addressCount" <*> o .: "allegoryHash") <|>
        (PSAllpayTransaction <$> o .: "inputs" <*> o .: "recipient" <*> o .: "amount" <*> o .: "change") <|>
        (Register <$> o .: "name" <*> (T.encodeUtf8 <$> o .: "xpubKey") <*> o .: "nutxo" <*> o .: "return" <*>
         o .: "addressCount") <|>
        (GetCoins <$> o .: "address")

data RPCResponseBody
    = RespXPubKey
          { rxpb :: Bool
          , addressCommitment :: String
          , ppUtxoCommitment :: String
          }
    | RespPSAllpayTransaction
          { serialisedTx :: ByteString
          , addressProof :: [(Bool, Hash256)]
          , utxoProof :: [(Bool, Hash256)]
          }
    | RespRegister
          { registrationTx :: ByteString
          }
    deriving (Generic, Show, Eq, Serialise)

instance ToJSON RPCResponseBody where
    toJSON (RespXPubKey rxpb ac uc) = object ["registered" .= rxpb, "addressCommitment" .= ac, "utxoCommitment" .= uc]
    toJSON (RespPSAllpayTransaction stx ap up) =
        object ["tx" .= (T.decodeUtf8 . B64.encode $ stx), "addressProof" .= ap, "utxoProof" .= up]
    toJSON (RespRegister stx) = object ["tx" .= (T.decodeUtf8 . B64.encode $ stx)]

-- data BlockRecord =
--     BlockRecord
--         { rbHeight :: Int
--         , rbHash :: String
--         , rbHeader :: String
--         }
--     deriving (Generic, Show, Hashable, Eq, Serialise)
-- data TxRecord =
--     TxRecord
--         { txId :: String
--         , txBlockInfo :: BlockInfo'
--         , txSerialized :: C.ByteString
--         }
--     deriving (Show, Generic, Hashable, Eq, Serialise)
-- data AddressOutputs =
--     AddressOutputs
--         { aoAddress :: String
--         , aoOutput :: OutPoint'
--         , aoBlockInfo :: BlockInfo'
--         , aoIsBlockConfirmed :: Bool
--         , aoIsOutputSpent :: Bool
--         , aoIsTypeReceive :: Bool
--         , aoOtherAddress :: String
--         , aoPrevOutpoint :: OutPoint'
--         , aoValue :: Int64
--         }
--     deriving (Show, Generic, Hashable, Eq, Serialise)
{-
data OutPoint' =
    OutPoint'
        { opTxHash :: String
        , opIndex :: Int
        }
    deriving (Show, Generic, Hashable, Eq, Serialise)

instance FromJSON OutPoint' where
    parseJSON (Object o) = (OutPoint' <$> o .: "txid" <*> o .: "index")

instance ToJSON OutPoint'
-}
-- data BlockInfo' =
--     BlockInfo'
--         { binfBlockHash :: String
--         , binfTxIndex :: Int
--         , binfBlockHeight :: Int
--         }
--     deriving (Show, Generic, Hashable, Eq, Serialise)
-- data MerkleBranchNode' =
--     MerkleBranchNode'
--         { nodeValue :: String
--         , isLeftNode :: Bool
--         }
--     deriving (Show, Generic, Hashable, Eq, Serialise)
data PubNotifyMessage =
    PubNotifyMessage
        { psBody :: String
        }
    deriving (Show, Generic, Eq, Serialise)

-- data PubSubMsg
--     = Subscribe'
--           { topic :: String
--           }
--     | Publish'
--           { topic :: String
--           , message :: PubNotifyMessage
--           }
--     | Notify'
--           { topic :: String
--           , message :: PubNotifyMessage
--           }
--     deriving (Show, Generic, Serialise)
data Tx' =
    Tx'
        { txVersion :: !Word32
        , txIn :: ![TxIn']
        , txOut :: ![TxOut]
        , txLockTime :: !Word32
        }
    deriving (Show, Read, Eq, Ord, Generic, Hashable, Serialise)

data TxIn' =
    TxIn'
        { prevOutput :: !OutPoint
        , scriptInput :: !ByteString
        , txInSequence :: !Word32
        , value :: !Word64
        }
    deriving (Eq, Show, Read, Ord, Generic, Hashable, Serialise)

instance ToJSON Tx' where
    toJSON (Tx' v i o l) = object ["version" .= v, "ins" .= i, "outs" .= o, "locktime" .= l]

instance ToJSON TxIn' where
    toJSON (TxIn' op scr seq val) = object ["outpoint" .= op, "script" .= scr, "sequence" .= seq, "value" .= val]

data ProxyProviderException
    = InvalidFaucetKeyException
    | UserAddressException
    | PoolAddressException
    | NexaResponseParseException
    | InvalidOpReturnHashException
    | RegistrationException
    | UserValidationException
    deriving (Show, Eq)

instance Exception ProxyProviderException

data GetUtxosByAddressResponse =
    GetUtxosByAddressResponse
        { nextCursor :: Maybe String
        , utxos :: [AddressOutputs]
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON GetUtxosByAddressResponse

data AddressOutputs =
    AddressOutputs
        { address :: String
        , outputTxHash :: String
        , outputIndex :: Int
        , txIndex :: Maybe Int
        , blockHash :: Maybe String
        , blockHeight :: Maybe Int
        , spendInfo :: Maybe SpendInfo
        , prevOutpoint :: [(OutPoint', Int32, Int64)]
        , value :: Int64
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON AddressOutputs

data SpendInfo =
    SpendInfo
        { spendingTxId :: String
        , spendingTxIndex :: Int32
        , spendingBlockHash :: String
        , spendingBlockHeight :: Int32
        , spendData :: [SpendInfo']
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON SpendInfo

data OutPoint' =
    OutPoint'
        { opTxHash :: String
        , opIndex :: Int
        }
    deriving (Show, Ord, Eq, Read, Generic, Serialise)

instance FromJSON OutPoint'

instance ToJSON OutPoint'

data SpendInfo' =
    SpendInfo'
        { spendingOutputIndex :: Int32
        , outputAddress :: DT.Text
        , value' :: Int64
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON SpendInfo'

data NexaRequest
    = RelayTxRequest
          { rawTx :: ByteString
          }
    | FindUriRequest
          { name :: [Int]
          , isProducer :: Bool
          }
    deriving (Show, Ord, Eq, Read, Generic)

instance ToJSON NexaRequest where
    toJSON (RelayTxRequest r) = object ["rawTx" .= (T.decodeUtf8 $ B64.encode r)]
    toJSON (FindUriRequest n p) = object ["name" .= n, "isProducer" .= p]

data RelayTxResponse =
    RelayTxResponse
        { txBroadcast :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON RelayTxResponse

data FindUriResponse =
    FindUriResponse
        { forName :: [Int]
        , uri :: String
        , protocol :: String
        , isConfirmed :: Bool
        , isProducer :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON FindUriResponse
