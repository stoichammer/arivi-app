{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module UtxoPool where

import Codec.Serialise
import Data.Aeson as A
import Data.ByteString.Lazy.UTF8 as BL
import Data.Hashable
import GHC.Generics
import GHC.IO.Handle
import System.Process

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

getPool :: String -> String -> IO (Maybe [ProxyProviderUtxo])
getPool addr apiKey = do
    (_, Just hout, _, _) <- createProcess (proc "curl" ["-s", endpoint, auth]) {std_out = CreatePipe}
    contents <- BL.fromString <$> hGetContents hout
    return (A.decode contents :: Maybe [ProxyProviderUtxo])
  where
    endpoint = "https://api.mattercloud.net/api/v3/test/address/" ++ addr ++ "/utxo"
    auth = " -H \"api_key: " ++ apiKey ++ "\""
