{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Types where

import ClassyPrelude
import Control.Monad.Logger
import Configuration
import Data.Pool (Pool)
import Database.Selda.Backend (SeldaConnection)
import Crypto.Random.Types (MonadRandom(..))

data App = App { config :: Config
               , database :: Pool SeldaConnection }
         deriving (Generic)

type AppM = LoggingT (ReaderT App IO)

instance MonadRandom AppM where
  getRandomBytes = lift . lift . getRandomBytes
