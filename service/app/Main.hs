{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( module Main
    ) where

import Arivi.Crypto.Utils.PublicKey.Signature as ACUPS
import Arivi.Crypto.Utils.PublicKey.Utils
import Arivi.Env
import Arivi.Network
import Arivi.P2P
import qualified Arivi.P2P.Config as Config
import Arivi.P2P.Kademlia.Types
import Arivi.P2P.P2PEnv as PE
import Arivi.P2P.PubSub.Types
import Arivi.P2P.RPC.Types
import Arivi.P2P.ServiceRegistry
import Codec.Serialise
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async, wait)
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types (parse)
import Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import Data.IORef
import Data.Int
import Data.Map.Strict as M
import Data.Maybe
import Data.String.Conv
import Data.Text
import qualified Data.Text.Encoding as DTE
import Data.Typeable
import Database.LevelDB
import LevelDB
import Network.Simple.TCP
import Network.Xoken.Keys.Extended
import qualified NodeConfig as NC
import Numeric (showHex)
import Service.Data
import Service.Env
import Service.Types
import StmContainers.Map as H
import System.Directory
import System.Environment (getArgs)
import TLSServer
import UtxoPool


newtype AppM a =
    AppM (ReaderT (ServiceEnv AppM ServiceResource ServiceTopic RPCMessage PubNotifyMessage) (LoggingT IO) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (ServiceEnv AppM ServiceResource ServiceTopic RPCMessage PubNotifyMessage)
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadLogger
             )

deriving instance MonadBase IO AppM

deriving instance MonadBaseControl IO AppM

instance HasAddressMap AppM where
    getAddressMap = asks (addressMap)

instance HasXPubInfoMap AppM where
    getXPubHashMap = asks (xpubInfoMap)

instance HasNodeConfig AppM where
    getNodeConfig = asks (nodeConfig)

instance HasNetworkEnv AppM where
    getEnv = asks (ariviNetworkEnv . nodeEndpointEnv . p2pEnv)

instance HasSecretKey AppM

instance HasKbucket AppM where
    getKb = asks (kbucket . kademliaEnv . p2pEnv)

instance HasStatsdClient AppM where
    getStatsdClient = asks (statsdClient . p2pEnv)

instance HasNodeEndpoint AppM where
    getEndpointEnv = asks (nodeEndpointEnv . p2pEnv)
    getNetworkConfig = asks (PE._networkConfig . nodeEndpointEnv . p2pEnv)
    getHandlers = asks (handlers . nodeEndpointEnv . p2pEnv)
    getNodeIdPeerMapTVarP2PEnv = asks (tvarNodeIdPeerMap . nodeEndpointEnv . p2pEnv)

instance HasPRT AppM where
    getPeerReputationHistoryTableTVar = asks (tvPeerReputationHashTable . prtEnv . p2pEnv)
    getServicesReputationHashMapTVar = asks (tvServicesReputationHashMap . prtEnv . p2pEnv)
    getP2PReputationHashMapTVar = asks (tvP2PReputationHashMap . prtEnv . p2pEnv)
    getReputedVsOtherTVar = asks (tvReputedVsOther . prtEnv . p2pEnv)
    getKClosestVsRandomTVar = asks (tvKClosestVsRandom . prtEnv . p2pEnv)

runAppM :: ServiceEnv AppM ServiceResource ServiceTopic RPCMessage PubNotifyMessage -> AppM a -> LoggingT IO a
runAppM env (AppM app) = runReaderT app env

defaultConfig :: FilePath -> IO ()
defaultConfig path = do
    (sk, _) <- ACUPS.generateKeyPair
    let bootstrapPeer =
            Peer
                ((fst . B16.decode)
                     "a07b8847dc19d77f8ef966ba5a954dac2270779fb028b77829f8ba551fd2f7ab0c73441456b402792c731d8d39c116cb1b4eb3a18a98f4b099a5f9bdffee965c")
                (NodeEndPoint "51.89.40.95" 5678 5678)
    let config =
            Config.Config
                5678
                5678
                sk
                [bootstrapPeer]
                (generateNodeId sk)
                "127.0.0.1"
                (Data.Text.pack (path <> "/node.log"))
                20
                5
                3
    Config.makeConfig config (path <> "/config.yaml")

runNode :: Config.Config -> NC.NodeConfig -> [FilePath] -> [ProxyProviderUtxo] -> IO ()
runNode config nodeConfig certPaths pool = do
    p2pEnv <- mkP2PEnv config undefined undefined [AriviSecureRPC] []
    que <- atomically $ newTChan
    mmap <- newTVarIO $ M.empty
    let net = NC.bitcoinNetwork nodeConfig
    -- read xpubKeys and build a HashMap
    allXPubKeys <-
        fmap (Prelude.map (Data.Text.unpack) . fromMaybe [] . Data.Aeson.decode . BSL.fromStrict) <$> getValue "names" :: IO (Maybe [String])
    xPubInfoMap <-
        case allXPubKeys of
            Just ks -> do
                res <-
                    traverse
                        (\name ->
                             fmap (fmap (name, ) . parse Prelude.id . decodeXPubInfo net) <$>
                             (getValue (DTE.encodeUtf8 . Data.Text.pack $ name)))
                        ks
                case sequence $ catMaybes res of
                    Success x -> pure $ M.fromList x
                    Data.Aeson.Error err -> do
                        print err
                        pure M.empty
            Nothing -> pure M.empty
    amr <- newTVarIO xPubInfoMap
    let addressMap =
            Prelude.foldl
                (\m (name, XPubInfo {..}) -> M.insert (xPubExport net key) (getAddressList key count) m)
                M.empty
                (M.toList xPubInfoMap)
    amT <- newTVarIO addressMap
    let serviceEnv = ServiceEnv EndPointEnv p2pEnv nodeConfig amT amr
    -- start TLS
    epHandler <- newTLSEndpointServiceHandler
    async $
        startTLSEndpoint epHandler (NC.endPointTLSListenIP nodeConfig) (NC.endPointTLSListenPort nodeConfig) certPaths
    runFileLoggingT (toS $ Config.logFile config) $ runAppM serviceEnv (handleNewConnectionRequest epHandler)
    return ()

main :: IO ()
main = do
    let path = "."
    b <- doesPathExist (path <> "/arivi-config.yaml")
    up <- unless b (defaultConfig path)
    config <- Config.readConfig (path <> "/arivi-config.yaml")
    nodeCnf <- NC.readConfig (path <> "/node-config.yaml")
    let certFP = NC.tlsCertificatePath nodeCnf
        keyFP = NC.tlsKeyfilePath nodeCnf
        csrFP = NC.tlsCertificateStorePath nodeCnf
    cfp <- doesFileExist certFP
    kfp <- doesFileExist keyFP
    csfp <- doesDirectoryExist csrFP
    unless (cfp && kfp && csfp) $ Prelude.error "Error: missing TLS certificate or keyfile"
    print "building proxy-provider utxo pool..."
    pool <- getPool (NC.poolAddress nodeCnf) (NC.apiAuthKey nodeCnf)
    case pool of
        Just p' -> do
            print $ "size of pool: " ++ show (Prelude.length p')
            -- launch node
            runNode config nodeCnf [certFP, keyFP, csrFP] p'
        Nothing -> do
            print "failed to build utxo pool!"
            return ()
