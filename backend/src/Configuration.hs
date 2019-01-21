{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
module Configuration where

import ClassyPrelude
import Dhall (Interpret)

data Pg = Pg { username :: Text
             , password :: Text
             , host :: Text
             , database :: Text
             , migrations :: Text }
        deriving (Show, Generic)

data Store = Filestore { path :: Text }
           | IPFS { common :: Text }
           deriving (Show, Generic)

data Config = Config { database :: Pg
                     , store :: Store
                     , port :: Integer }
            deriving (Show, Generic)

instance Interpret Pg
instance Interpret Store
instance Interpret Config

