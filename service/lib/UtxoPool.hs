{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module UtxoPool where

import Codec.Serialise
import Data.Aeson as A
import Data.ByteString.Lazy.UTF8 as BL
import Data.Hashable
import Data.Int
import GHC.Generics
import GHC.IO.Handle
import System.IO
import System.Process

--
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

--
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

--
getPool :: FilePath -> IO (Maybe [ProxyProviderUtxo])
getPool txFile = do
    fh <- openFile txFile ReadMode
    txJson <- hGetContents fh
    let r = A.decode (BL.fromString txJson) :: (Maybe NexaTx)
    case r of
        Nothing -> return $ Nothing
        (Just dTx) -> do
            return $
                Just $
                (\(NexaTxOut scr val addr index) -> (Unspent addr (txId $ innerTx dTx) index val scr)) <$>
                (txOuts $ txInsOuts $ innerTx dTx)
