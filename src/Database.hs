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
  , SeldaT )
  where

import Data.Generics.Product
import Control.Lens (view)
import Data.Pool (Pool, withResource)
import Database.Selda.Backend (SeldaConnection, runSeldaT, SeldaT)
import Database.Selda (query, select)
import Database.Selda.Generic (gen, fromRel, fromRels, toRel)
import ClassyPrelude

type DBLike r m = (MonadIO m, MonadReader r m, MonadBaseControl IO m, MonadMask m, HasField' "database" r (Pool SeldaConnection))

runDB :: DBLike r m => SeldaT m a -> m a
runDB q = do
  pool <- view (field @"database")
  withResource pool $ \conn ->
    runSeldaT q conn
