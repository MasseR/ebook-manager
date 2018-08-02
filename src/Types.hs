{-# Language NoImplicitPrelude #-}
module Types where

import ClassyPrelude
import Control.Monad.Logger
import Configuration

newtype App = App { config :: Config }

type AppM = LoggingT (ReaderT App IO)
