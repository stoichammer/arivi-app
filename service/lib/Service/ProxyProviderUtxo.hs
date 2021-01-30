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

getFromPool' :: (HasService env m, MonadIO m) => Int -> m [String]
getFromPool' count = do
    utxoPoolTvar <- getUtxoPool
    utxoPool <- liftIO $ readTVarIO utxoPoolTvar
    let ops = fst . L.splitAt count $ M.keys utxoPool
    let committedUtxos = zip ops $ fromJust <$> flip M.lookup utxoPool <$> ops
    -- remove utxos from pool
    res0 <- liftIO $ sequence $ (\op -> atomically $ modifyTVar utxoPoolTvar (M.delete op)) <$> ops
    -- add utxos to committed set
    committedUtxosTVar <- getCommittedUtxos
    res1 <-
        liftIO $ sequence $ (\(k, v) -> atomically $ modifyTVar committedUtxosTVar (M.insert k v)) <$> committedUtxos
    -- return commitment
    return ops

getFromPool :: (HasService env m, MonadIO m) => Int -> m [String]
getFromPool count = do
    utxoPoolTvar <- getUtxoPool
    utxoPool <- liftIO $ readTVarIO utxoPoolTvar
    let ops = fst . L.splitAt count $ M.keys utxoPool
    -- transfer to committed utxos set
    transferToCommitted ops
    -- return commitment
    return ops

transferToCommitted :: (HasService env m, MonadIO m) => [String] -> m ()
transferToCommitted outpoints = do
    utxoPoolTVar <- getUtxoPool
    utxoPool <- liftIO $ readTVarIO utxoPoolTVar
    let cu = zip outpoints $ fromJust <$> flip M.lookup utxoPool <$> outpoints
    -- remove utxos from pool
    liftIO $ sequence $ (\outpoint -> atomically $ modifyTVar utxoPoolTVar (M.delete outpoint)) <$> outpoints
    -- add utxos to committed set
    commitUtxosTVar <- getCommittedUtxos
    liftIO $ sequence $ (\(k, v) -> atomically $ modifyTVar commitUtxosTVar (M.insert k v)) <$> cu
    return ()

getCommittedUtxo :: (HasService env m, MonadIO m) => String -> m (Maybe ProxyProviderUtxo)
getCommittedUtxo op = do
    committedUtxosTVar <- getCommittedUtxos
    committedUtxos <- liftIO $ readTVarIO committedUtxosTVar
    return $ M.lookup op committedUtxos
