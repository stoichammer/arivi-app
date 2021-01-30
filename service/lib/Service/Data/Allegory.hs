{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Service.Data.Allegory where

import Codec.Serialise
import Data.Aeson
import Data.Yaml ()
import GHC.Generics

data Allegory =
    Allegory
        { version :: !Int
        , name :: ![Int]
        , action :: !Action
        }
    deriving (Show, Generic, Eq, Serialise)

data Action
    = ProducerAction
          { producerInput :: !Index
          , producerOutput :: !ProducerOutput
          , pOwnerOutput :: !(Maybe OwnerOutput)
          , extensions :: ![Extension]
          }
    | OwnerAction
          { ownerInput :: !Index
          , ownerOutput :: !OwnerOutput
          , oProxyProviders :: ![ProxyProvider]
          }
    deriving (Show, Generic, Eq, Serialise)

data ProducerOutput =
    ProducerOutput
        { producer :: !Index
        , pVendorEndpoint :: !(Maybe Endpoint)
        }
    deriving (Show, Generic, Eq, Serialise)

data OwnerOutput =
    OwnerOutput
        { owner :: !Index
        , oVendorEndpoint :: !(Maybe Endpoint)
        }
    deriving (Show, Generic, Eq, Serialise)

data Index =
    Index
        { index :: !Int
        }
    deriving (Show, Generic, Eq, Serialise)

data Extension
    = OwnerExtension
          { ownerOutputEx :: !OwnerOutput
          , codePoint :: !Int
          }
    | ProducerExtension
          { producerOutputEx :: !ProducerOutput
          , codePoint :: !Int
          }
    deriving (Show, Generic, Eq, Serialise)

data ProxyProvider =
    ProxyProvider
        { service :: !String
        , mode :: !String
        , endpoint :: !Endpoint
        , registration :: !Registration
        }
    deriving (Show, Generic, Eq, Serialise)

data Endpoint =
    Endpoint
        { protocol :: !String
        , uri :: !String
        }
    deriving (Show, Generic, Eq, Serialise)

data Registration =
    Registration
        { addressCommitment :: !String
        , utxoCommitment :: !String
        , signature :: !String
        , expiry :: !Int
        }
    deriving (Show, Generic, Eq, Serialise)

instance ToJSON ProxyProvider

instance ToJSON Registration

instance ToJSON Endpoint
