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

import Data.Maybe
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.List as L
import Service.Env
import UtxoPool
import Data.Map.Strict as M


getFromPool :: (HasService env m, MonadIO m) => Int -> m [String]
getFromPool count = do
    utxoPoolTvar <- getUtxoPool
    utxoPool <- liftIO $ readTVarIO utxoPoolTvar
    let ops = fst . L.splitAt count $ M.keys utxoPool
    let committedUtxos = zip ops $ fromJust <$> flip M.lookup utxoPool <$> ops
    -- remove utxos from pool
    res0 <- liftIO $ sequence $ (\op -> atomically $ modifyTVar utxoPoolTvar (M.delete op)) <$> ops
    -- add utxos to committed set
    committedUtxosTVar <- getCommittedUtxos
    res1 <- liftIO $ sequence $ (\(k, v) -> atomically $ modifyTVar committedUtxosTVar (M.insert k v)) <$> committedUtxos
    -- return commitment
    return ops 

getCommittedUtxo :: (HasService env m, MonadIO m) => String -> m (Maybe ProxyProviderUtxo)
getCommittedUtxo op = do
    committedUtxosTVar <- getCommittedUtxos
    committedUtxos <- liftIO $ readTVarIO committedUtxosTVar
    return $ M.lookup op committedUtxos
