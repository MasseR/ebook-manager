{-# Language NoImplicitPrelude #-}
module Types where

import ClassyPrelude
import Control.Monad.Logger

data App = App

type AppM = LoggingT (ReaderT App IO)
