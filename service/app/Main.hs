{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Main
    ( module Main
    ) where

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
import Crypto.Secp256k1
import Data.Aeson
import Data.Aeson.Types (parse)
import Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import qualified Data.ByteString.UTF8 as BSU
import Data.Function
import Data.IORef
import Data.Int
import qualified Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Data.String.Conv
import Data.Text
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import Data.Typeable
import Database.LevelDB
import HTTP.Server
import LevelDB
import Network.Simple.TCP
import Network.Xoken.Address
import Network.Xoken.Constants
import Network.Xoken.Keys
import qualified NodeConfig as NC
import Numeric (showHex)
import Service.Env
import qualified Snap as Snap
import StmContainers.Map as H
import System.Directory
import System.Environment (getArgs)
import System.IO as IO (putStrLn)
import qualified System.Logger as LG
import System.Posix.Daemon
import UtxoPool

runNode :: NC.NodeConfig -> [FilePath] -> [ProxyProviderUtxo] -> IO ()
runNode nodeConfig certPaths pool = do
    IO.putStrLn "------------------------------"
    IO.putStrLn "Starting Allpay Proxy-Provider"
    IO.putStrLn "------------------------------"
    let addresses =
            (fromJust . addrToString (NC.bitcoinNetwork nodeConfig) . pubKeyAddr . derivePubKeyI . wrapSecKey True) <$>
            [NC.faucetSecKey nodeConfig, NC.poolSecKey nodeConfig]
    IO.putStrLn $ "Faucet address: " <> (show $ addresses !! 0)
    IO.putStrLn $ "UTXO pool address: " <> (show $ addresses !! 1)
    IO.putStrLn $ "Size of UTXO pool: " <> show (Prelude.length pool)
    let net = NC.bitcoinNetwork nodeConfig
    --
    --
    subscriberHashes <-
        fmap (Prelude.map (Data.Text.unpack) . fromMaybe [] . Data.Aeson.decode . BSL.fromStrict) <$>
        getValue "subscribers" :: IO (Maybe [String])
    subMap <-
        case subscriberHashes of
            Nothing -> return M.empty
            Just hashes -> do
                res <-
                    traverse
                        (\hash ->
                             fmap (fmap (hash, ) . parse Prelude.id . decodeSubscriber net) <$>
                             (getValue (DTE.encodeUtf8 . Data.Text.pack $ hash)))
                        hashes
                case sequence $ catMaybes res of
                    Success subs -> return $ M.fromList subs
                    Data.Aeson.Error e -> do
                        print e
                        return M.empty
    --
    --
    -- build utxo pool
    let (utxoPool, committedUtxos) =
            L.partition (\opu -> (fst opu `L.notElem` (L.concat $ (\sub -> ppUtxos sub) <$> M.elems subMap))) $
            (\utxo -> (txid utxo ++ ":" ++ (show $ outputIndex utxo), utxo)) <$> pool
    up <- newTVarIO $ M.fromList utxoPool
    cu <- newTVarIO $ M.fromList committedUtxos
    sub <- newTVarIO subMap
    lg <-
        LG.new
            (LG.setOutput
                 (LG.Path $ DT.unpack $ NC.logFileName nodeConfig)
                 (LG.setLogLevel (NC.logLevel nodeConfig) LG.defSettings))
    let allpayProxyEnv = AllpayProxyEnv nodeConfig sub up cu lg
    let snapConfig =
            Snap.defaultConfig & Snap.setSSLBind (DTE.encodeUtf8 $ DT.pack $ NC.endPointListenIP nodeConfig) &
            Snap.setSSLPort (fromEnum $ NC.endPointListenPort nodeConfig) &
            Snap.setSSLKey (certPaths !! 1) &
            Snap.setSSLCert (L.head certPaths) &
            Snap.setSSLChainCert False
    Snap.serveSnaplet snapConfig (appInit allpayProxyEnv)

main :: IO ()
main = do
    let path = "."
    nodeCnf <- NC.readConfig (path <> "/allpay-proxy-config.yaml")
    let certFP = NC.tlsCertificatePath nodeCnf
        keyFP = NC.tlsKeyfilePath nodeCnf
        csrFP = NC.tlsCertificateStorePath nodeCnf
    cfp <- doesFileExist certFP
    kfp <- doesFileExist keyFP
    csfp <- doesDirectoryExist csrFP
    unless (cfp && kfp && csfp) $ Prelude.error "Error: missing TLS certificate or keyfile"
    pool <-
        getPoolFromAddress
            (NC.bitcoinNetwork nodeCnf)
            (NC.nexaHost nodeCnf)
            (NC.poolAddress nodeCnf)
            (NC.nexaSessionKey nodeCnf)
    -- launch node as daemon
    let pid = "/tmp/aaproxy.pid.0"
    runDetached (Just pid) (ToFile "aaproxy.log") $ runNode nodeCnf [certFP, keyFP, csrFP] pool
