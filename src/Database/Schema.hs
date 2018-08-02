{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
module Database.Schema where

import ClassyPrelude
import Database.Selda.Generic

data User = User { email :: Text
                 , username :: Text
                 , password :: ByteString }
          deriving (Show, Generic)

users :: GenTable User
users = genTable "users" [ email :- primaryGen ]
