{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}
module Database.Schema where

import ClassyPrelude
import Database.Selda.Generic
import Database.Selda
import Database.Selda.Backend

data User pass = User { email :: Text
                      , username :: Text
                      , role :: Role
                      , password :: pass }
          deriving (Show, Generic)

data Role = UserRole | AdminRole deriving (Show, Read, Enum, Bounded, Typeable)

instance SqlType Role where
  mkLit = LCustom . LText . pack . show
  fromSql sql = case sql of
                     SqlString x -> fromMaybe (error "fromSql: Not a valid role") . readMay . unpack $ x
                     _ -> error "fromSql: Not a valid role"

  defaultValue = mkLit minBound

users :: GenTable (User ByteString)
users = genTable "users" [ email :- primaryGen ]
