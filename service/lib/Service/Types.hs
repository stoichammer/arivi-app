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
        (Register <$> o .: "name" <*> (T.encodeUtf8 <$> o .: "xpubKey") <*> o .: "addressCount") <|>
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
          { opReturnScript :: ByteString
          , registrationFee :: Int
          , paymentAddress :: String
          }
    deriving (Generic, Show, Eq, Serialise)

instance ToJSON RPCResponseBody where
    toJSON (RespXPubKey rxpb ac uc) = object ["registered" .= rxpb, "addressCommitment" .= ac, "utxoCommitment" .= uc]
    toJSON (RespPSAllpayTransaction stx ap up) =
        object ["tx" .= (T.decodeUtf8 . B64.encode $ stx), "addressProof" .= ap, "utxoProof" .= up]
    toJSON (RespRegister opret fee addr) =
        object ["opReturn" .= (T.decodeUtf8 opret), "registrationFeeSats" .= fee, "paymentAddress" .= addr]

data PubNotifyMessage =
    PubNotifyMessage
        { psBody :: String
        }
    deriving (Show, Generic, Eq, Serialise)

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
    | TxParseException
    deriving (Show, Eq)

instance Exception ProxyProviderException

data RegValidationException
    = RawTxParseException
    | InvalidNameException
    | PaymentAddressException
    deriving (Show, Eq)

instance Exception RegValidationException

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
    | NexaNameRequest
          { name :: [Int]
          , isProducer :: Bool
          }
    deriving (Show, Ord, Eq, Read, Generic)

instance ToJSON NexaRequest where
    toJSON (RelayTxRequest r) = object ["rawTx" .= (T.decodeUtf8 $ B64.encode r)]
    toJSON (NexaNameRequest n p) = object ["name" .= n, "isProducer" .= p]

data RelayTxResponse =
    RelayTxResponse
        { txBroadcast :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON RelayTxResponse

data ResellerUriResponse =
    ResellerUriResponse
        { forName :: [Int]
        , uri :: String
        , protocol :: String
        , isConfirmed :: Bool
        , isProducer :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON ResellerUriResponse

data NameOutpointResponse =
    NameOutpointResponse
        { forName :: [Int]
        , script :: String
        , isConfirmed :: Bool
        , outPoint :: OutPoint'
        , isProducer :: Bool
        }
    deriving (Show, Ord, Eq, Read, Generic)

instance FromJSON NameOutpointResponse
