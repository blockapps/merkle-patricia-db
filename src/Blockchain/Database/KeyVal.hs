{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Blockchain.Database.KeyVal (
    PointedKeyValDB(..),
    KeyValMPLevelDB,
    KeyValMPMap
  ) where

import           Blockchain.Database.MerklePatricia
import           Blockchain.Database.MerklePatriciaMem
import           Control.Monad.Trans
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State

{-
  A general interface for a key value "database" with a distinguished element
  representing a "snapshot" or "summary" of the database. The "snapshot" can also
  include the database handle or connection string.

  Below, a and b represent key and value types respectively, and c is the type of the
  "snapshot".

  The unfortunate parameter t represents in some cases a file path for the on disk
  db.
-}

class (Monad m) => PointedKeyValDB m a b c t | c -> t where
    getKV :: Monad m => a-> m (Maybe b)
    putKV :: Monad m => (a,b) -> m c
    deleteKV :: Monad m => a -> m c
    emptyKV :: Monad m => t -> m c

type KeyValMPLevelDB m = StateT MPDB (ResourceT m)
type KeyValMPMap m = StateT MPMem m

data Void

instance (
           Monad m,
           MonadBaseControl IO m,
           MonadThrow m,
           MonadIO m
         )
        => PointedKeyValDB (KeyValMPLevelDB m) Key Val MPDB String where

    getKV key = do
        db <- get
        runResourceT $ getKeyVal db key

    putKV kvPair = do
        db <- get
        runResourceT $ putKeyVal db (fst kvPair) (snd kvPair)

    deleteKV key = do
        db <- get
        runResourceT $ deleteKey db key

    emptyKV path = liftIO $ runResourceT $ openMPDB path  -- fix

instance (
           Monad m
         )
        => PointedKeyValDB (KeyValMPMap m) Key Val MPMem Void where

    getKV key = do
        db <- get
        getKeyValMem db key

    putKV kvPair = do
        db <- get
        putKeyValMem db (fst kvPair) (snd kvPair)

    deleteKV key = do
        db <- get
        deleteKeyMem db key

    emptyKV _ = return initializeBlankMem             -- fix
