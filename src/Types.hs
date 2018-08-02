{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
module Types where

import ClassyPrelude
import Control.Monad.Logger
import Configuration
import Data.Pool (Pool)
import Database.Selda.Backend (SeldaConnection)

data App = App { config :: Config
               , database :: Pool SeldaConnection }
         deriving (Generic)

type AppM = LoggingT (ReaderT App IO)
