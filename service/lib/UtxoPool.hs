{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module UtxoPool where

import Codec.Serialise
import Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.UTF8 as BL
import Data.Hashable
import Data.Int
import GHC.Generics
import GHC.IO.Handle
import System.IO
import System.Process

import Network.Connection
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Simple

data ProxyProviderUtxo =
    Unspent
        { address :: String
        , txid :: String
        , outputIndex :: Int
        , value :: Int
        , scriptPubKey :: String
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON ProxyProviderUtxo where
    parseJSON (Object o) =
        (Unspent <$> o .: "address" <*> o .: "txid" <*> o .: "vout" <*> o .: "value" <*> o .: "scriptPubKey")

instance ToJSON ProxyProviderUtxo where
    toJSON (Unspent a t o v s) =
        object ["address" .= a, "txid" .= t, "outputIndex" .= o, "value" .= v, "scriptPubKey" .= s]

data NexaTx =
    NexaTx
        { innerTx :: NexaInnerTx
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON NexaTx where
    parseJSON (Object o) = (NexaTx <$> o .: "tx")

data NexaInnerTx =
    NexaInnerTx
        { txInsOuts :: NTxInsOuts
        , txId :: String
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON NexaInnerTx where
    parseJSON (Object o) = (NexaInnerTx <$> o .: "tx" <*> o .: "txId")

data NTxInsOuts =
    NTxInsOuts
        { txOuts :: [NexaTxOut]
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON NTxInsOuts where
    parseJSON (Object o) = (NTxInsOuts <$> o .: "txOuts")

data NexaTxOut =
    NexaTxOut
        { lockingScript :: String
        , value' :: Int
        , address' :: String
        , outIndex :: Int
        }
    deriving (Show, Eq, Hashable, Generic, Serialise)

instance FromJSON NexaTxOut where
    parseJSON (Object o) =
        (NexaTxOut <$> o .: "lockingScript" <*> o .: "value" <*> o .: "address" <*> o .: "outputIndex")

getPool :: String -> String -> IO (Maybe [ProxyProviderUtxo])
getPool poolTxId sessionKey = do
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing
    endpoint <- parseRequest $ "https://127.0.0.1:9091/v1/transaction/" ++ poolTxId
    let request =
            setRequestHeader "Authorization" [B.pack $ "Bearer " ++ sessionKey] $
            setRequestManager manager $ setRequestMethod "GET" $ endpoint
    response <- httpLBS request
    case A.decode (getResponseBody response) :: Maybe NexaTx of
        Nothing -> return $ Nothing
        (Just dTx) -> do
            return $
                Just $
                (\(NexaTxOut scr val addr index) -> (Unspent addr (txId $ innerTx dTx) index val scr)) <$>
                (txOuts $ txInsOuts $ innerTx dTx)

