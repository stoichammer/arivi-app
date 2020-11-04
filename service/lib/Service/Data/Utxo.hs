{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Service.Data.Utxo where

import GHC.Generics
import Data.Hashable
import Codec.Serialise

data Utxo =
    Utxo
    {   txid :: String
    ,   outputIndex :: String
    } deriving (Show, Eq, Hashable, Generic, Serialise)

 
