{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Service.ProxyProviderUtxo where

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.List as L
import Service.Env
import UtxoPool

getFromPool :: (HasService env m, MonadIO m) => Int -> m [ProxyProviderUtxo]
getFromPool count = do
    utxoPoolTvar <- getUtxoPool
    pool <- liftIO $ readTVarIO utxoPoolTvar
    let (commit, pool') = L.splitAt count pool
    liftIO $ atomically $ writeTVar utxoPoolTvar pool'
    return commit

