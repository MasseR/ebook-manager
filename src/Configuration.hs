{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
module Configuration where

import ClassyPrelude
import Dhall (Interpret)

data Pg = Pg { username :: Text
             , password :: Text
             , host :: Text
             , database :: Text }
        deriving (Show, Generic)


newtype Config = Config { database :: Pg } deriving (Show, Generic)

instance Interpret Pg
instance Interpret Config
