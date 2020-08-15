{-# LANGUAGE OverloadedStrings #-}

module UtxoPool where

import Data.Aeson as A
import Data.ByteString.Lazy.UTF8 as BL
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
    deriving (Show, Eq)

instance FromJSON ProxyProviderUtxo where
    parseJSON (Object o) =
        (Unspent <$> o .: "address" <*> o .: "txid" <*> o .: "vout" <*> o .: "value" <*> o .: "scriptPubKey")

getPool :: String -> String -> IO (Maybe [ProxyProviderUtxo])
getPool addr apiKey = do
    (_, Just hout, _, _) <- createProcess (proc "curl" ["-s", endpoint, auth]) {std_out = CreatePipe}
    contents <- BL.fromString <$> hGetContents hout
    return (A.decode contents :: Maybe [ProxyProviderUtxo])
  where
    endpoint = "https://api.mattercloud.net/api/v3/test/address/" ++ addr ++ "/utxo"
    auth = " -H \"api_key: " ++ apiKey ++ "\""
