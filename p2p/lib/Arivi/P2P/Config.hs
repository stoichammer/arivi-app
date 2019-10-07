{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arivi.P2P.Config
    ( module Arivi.P2P.Config
    ) where

import           Arivi.P2P.Kademlia.Types 

import           Control.Exception
import           Crypto.Error             (throwCryptoError)
import           Crypto.PubKey.Ed25519
import qualified Data.ByteArray           as BA
import           Data.ByteString.Char8
import           Data.Text                as T
import           Data.Yaml
import           GHC.Generics
import           Network.Socket


    
data Config = Config
    { tcpPort                      :: PortNumber
    , udpPort                      :: PortNumber
    , secretKey                    :: SecretKey
    , trustedPeers                 :: [Peer]
    , myNodeId                     :: ByteString
    , myIp                         :: String
    , logFile                      :: T.Text
    , sbound                       :: Int
    , pingThreshold                :: Int
    , kademliaConcurrencyFactor    :: Int
    } deriving (Show, Generic)

instance FromJSON ByteString where
    parseJSON =
        withText "ByteString" $ \t ->
            pure $ Data.ByteString.Char8.pack (T.unpack t)

        
-- instance FromJSON Peer 

newtype PeerList = PeerList {peerList :: [Peer]}


instance FromJSON PeerList where
    parseJSON (Object o) = PeerList <$> o .: "trustedPeers"
    parseJSON _ = error "Can't parse PeerList"

                           
instance FromJSON NodeEndPoint where
    parseJSON  (Object v) = NodeEndPoint <$>
                           v .: "nodeIp" <*>
                           v .: "udpPort" <*>
                           v .: "tcpPort"
    parseJSON _ = error "Can't parse NodeEndPoint from YAML"

--instance FromJSON Peer where
--    parseJSON (Object o) = do
--        v <- Prelude.head <$> o .: "trustedPeers"
--        --Peer <$> v .: "nodeID" <*> v .: "endPoint"
--        Peer <$> ((,) <$> v .: "nodeID" <*> v .: "endPoint")
        
--    parseJSON _ = error "Can't parse "

instance FromJSON Peer where
    parseJSON  (Object v) = Peer <$>
                            v .: "nodeID" <*>
                            v .: "endPoint" 
    parseJSON _ = error "Can't parse Peer"


    
instance FromJSON PortNumber where
    parseJSON v = fromInteger <$> parseJSON v

instance FromJSON SecretKey where
    parseJSON v =
        throwCryptoError . Crypto.PubKey.Ed25519.secretKey <$>
        (parseJSON v :: Parser ByteString)

instance FromJSON Config

instance ToJSON ByteString where
    toJSON a = String $ T.pack (Data.ByteString.Char8.unpack a)

instance ToJSON Peer

instance ToJSON NodeEndPoint

instance ToJSON PortNumber where
    toJSON = Number . fromInteger . toInteger

instance ToJSON SecretKey where
    toJSON sk = toJSON (BA.convert sk :: ByteString)

instance ToJSON Config

makeConfig :: Config -> FilePath -> IO ()
makeConfig config configPath = encodeFile configPath config

readConfig :: FilePath -> IO Config
readConfig path = do
    config <- decodeFileEither path :: IO (Either ParseException Config)
    case config of
        Left e    -> throw e
        Right con -> return con
