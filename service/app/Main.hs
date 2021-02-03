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
import Network.Xoken.Constants
import Network.Xoken.Keys
import qualified NodeConfig as NC
import Numeric (showHex)
import Service.Env
import qualified Snap as Snap
import StmContainers.Map as H
import System.Directory
import System.Environment (getArgs)
import UtxoPool

runNode :: NC.NodeConfig -> [FilePath] -> [ProxyProviderUtxo] -> IO ()
runNode nodeConfig certPaths pool = do
    let net = NC.bitcoinNetwork nodeConfig
    print $ "sec key: " ++ show (NC.poolSecKey nodeConfig)
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
    -- build utxo pool
    let (utxoPool, committedUtxos) =
            L.partition
                (\opu -> (fst opu `L.notElem` (L.concat $ (\xpi -> utxoCommitment xpi) <$> M.elems xPubInfoMap))) $
            (\utxo -> (txid utxo ++ ":" ++ (show $ outputIndex utxo), utxo)) <$> pool
    up <- newTVarIO $ M.fromList utxoPool
    cu <- newTVarIO $ M.fromList committedUtxos
    amr <- newTVarIO xPubInfoMap
    let addressMap =
            Prelude.foldl
                (\m (name, XPubInfo {..}) -> M.insert (xPubExport net key) (getAddressList key count) m)
                M.empty
                (M.toList xPubInfoMap)
    amT <- newTVarIO addressMap
    let allpayProxyEnv = AllpayProxyEnv nodeConfig amT amr up cu
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
    print $ "size of pool: " ++ show (Prelude.length pool)
    -- launch node
    runNode nodeCnf [certFP, keyFP, csrFP] pool
