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

newtype Store = Store { path :: Text } deriving (Show, Generic)

data Config = Config { database :: Pg
                     , store :: Store }
            deriving (Show, Generic)

instance Interpret Pg
instance Interpret Store
instance Interpret Config
