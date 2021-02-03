{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Service.ProxyProviderUtxo where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.List as L
import Data.Map.Strict as M
import Data.Maybe
import Service.Env
import UtxoPool

getFromPool :: (HasService env m, MonadIO m) => Int -> m [String]
getFromPool count = do
    utxoPoolTVar <- getUtxoPool
    utxoPool <- liftIO $ readTVarIO utxoPoolTVar
    let ops = fst . L.splitAt count $ M.keys utxoPool
    -- transfer to committed utxos set
    let cu = zip ops $ fromJust <$> flip M.lookup utxoPool <$> ops
    -- remove utxos from pool
    liftIO $ sequence $ (\outpoint -> atomically $ modifyTVar utxoPoolTVar (M.delete outpoint)) <$> ops
    -- add utxos to committed set
    commitUtxosTVar <- getCommittedUtxos
    liftIO $ sequence $ (\(k, v) -> atomically $ modifyTVar commitUtxosTVar (M.insert k v)) <$> cu
    -- return commitment
    return ops

putBackInPool :: (HasService env m, MonadIO m) => [String] -> m ()
putBackInPool ops = do
    committedTVar <- getCommittedUtxos
    committedUtxos <- liftIO $ readTVarIO committedTVar
    -- get committed utxos
    let cu = zip ops $ fromJust <$> flip M.lookup committedUtxos <$> ops
    -- remove from committed pool
    liftIO $ sequence $ (\outpoint -> atomically $ modifyTVar committedTVar (M.delete outpoint)) <$> ops
    -- add to utxo pool
    utxoPoolTVar <- getUtxoPool
    liftIO $ sequence $ (\(k, v) -> atomically $ modifyTVar utxoPoolTVar (M.insert k v)) <$> cu
    return ()

getCommittedUtxo :: (HasService env m, MonadIO m) => String -> m (Maybe ProxyProviderUtxo)
getCommittedUtxo op = do
    committedUtxosTVar <- getCommittedUtxos
    committedUtxos <- liftIO $ readTVarIO committedUtxosTVar
    return $ M.lookup op committedUtxos
