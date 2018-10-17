{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language ConstraintKinds #-}
module Database
  ( DBLike
  , runDB
  , query
  , select
  , gen
  , fromRel
  , fromRels
  , toRel
  , transaction
  , SeldaT )
  where

import ClassyPrelude
import Control.Lens (view)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Generics.Product
import Data.Pool (Pool, withResource)
import Database.Selda (query, select, transaction)
import Database.Selda.Backend (SeldaConnection, runSeldaT, SeldaT)
import Database.Selda.Generic (gen, fromRel, fromRels, toRel)

type DBLike r m = (MonadBaseControl IO m, MonadIO m, MonadReader r m, HasField "database" r r (Pool SeldaConnection) (Pool SeldaConnection), MonadMask m)

runDB :: DBLike r m => SeldaT m a -> m a
runDB q = do
  pool <- view (field @"database")
  withResource pool $ \conn ->
    runSeldaT q conn
