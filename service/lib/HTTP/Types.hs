{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module HTTP.Types where

import Codec.Serialise
import Control.Applicative
import qualified Control.Exception as CE
import Control.Lens (makeLenses)
import qualified Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Crypto.Secp256k1
import Data.Aeson
import Data.ByteString
import Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Char as Char
import qualified Data.HashTable.IO as H
import Data.Hashable
import Data.Int
import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Data.Word
import GHC.Generics
import Network.HTTP.Req
import Network.Xoken.Block
import Network.Xoken.Crypto.Hash
import Prelude
import Service.Env
import Service.Types (OutPoint')
import Snap

data App =
    App
        { _env :: AllpayProxyEnv
        }

-- instance HasBitcoinP2P (Handler App App) where
--     getBitcoinP2P = bitcoinP2PEnv <$> gets _env
instance HasAddressMap (Handler App App) where
    getAddressMap = asks (addressMap . _env)

instance HasXPubInfoMap (Handler App App) where
    getXPubHashMap = asks (xpubInfoMap . _env)

instance HasUtxoPool (Handler App App) where
    getUtxoPool = asks (utxoPool . _env)

instance HasCommittedUtxos (Handler App App) where
    getCommittedUtxos = asks (committedUtxos . _env)

instance HasNodeConfig (Handler App App) where
    getNodeConfig = asks (nodeConfig . _env)

instance MC.MonadThrow (Handler App App) where
    throwM = liftIO . CE.throwIO

instance MonadUnliftIO (Handler App App) --where

instance MonadHttp (Handler App App) where
    handleHttpException = MC.throwM

-- Request & Response Types
--
data ReqParams'
    = Register
          { rName :: [Int]
          , rXpk :: ByteString
          , rCount :: Int
          }
    | RelayRegistrationTx
          { rTx :: ByteString
          }
    | PSAllpayTransaction
          { inputs :: [(OutPoint', Int64)]
          , recipient :: String
          , amount :: Int64
          , change :: String
          }
    deriving (Generic, Show, Eq, Serialise, ToJSON)

instance FromJSON ReqParams' where
    parseJSON (Object o) =
        (PSAllpayTransaction <$> o .: "inputs" <*> o .: "recipient" <*> o .: "amount" <*> o .: "change") <|>
        (RelayRegistrationTx . B64.decodeLenient . T.encodeUtf8 <$> o .: "rawTx") <|>
        (Register <$> o .: "name" <*> (T.encodeUtf8 <$> o .: "xpubKey") <*> o .: "addressCount")

data ResponseBody
    = RespPSAllpayTransaction
          { serialisedTx :: ByteString
          , addressProof :: [(Bool, Hash256)]
          , utxoProof :: [(Bool, Hash256)]
          }
    | RespRegister
          { opReturnScript :: ByteString
          , registrationFeeSats :: Int64
          , paymentAddress :: String
          }
    | RespRelayRegistrationTx
          { result :: Bool
          }
    | RespGiveCoins
          { result :: Bool
          }
    deriving (Generic, Show, Hashable, Eq, Serialise)

instance ToJSON ResponseBody where
    toJSON (RespPSAllpayTransaction stx ap up) =
        object ["tx" .= (T.decodeUtf8 . B64.encode $ stx), "addressProof" .= ap, "utxoProof" .= up]
    toJSON (RespRegister opret fee addr) =
        object ["opReturn" .= (opret), "registrationFeeSats" .= fee, "paymentAddress" .= addr]
    toJSON (RespRelayRegistrationTx rTx) = object ["success" .= rTx]
    toJSON (RespGiveCoins r) = object ["success" .= r]

makeLenses ''App
