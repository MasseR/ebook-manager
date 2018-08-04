{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
module Types
  ( App(..)
  , AppM
  -- Figure out how to re-export instances
  ) where

import ClassyPrelude
import Control.Monad.Logger
import Configuration
import Data.Pool (Pool)
import Database.Selda.Backend (SeldaConnection)
import Servant.Auth.Server as SAS ()
import Crypto.JOSE.JWK (JWK)

data App = App { config :: Config
               , database :: Pool SeldaConnection
               , jwk :: JWK }
         deriving (Generic)

type AppM = LoggingT (ReaderT App IO)
