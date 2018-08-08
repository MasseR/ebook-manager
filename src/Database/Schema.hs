{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}
{-# Language GeneralizedNewtypeDeriving #-}
module Database.Schema where

import ClassyPrelude
import Database.Selda.Generic
import Database.Selda
import Database.Selda.Backend

import Data.Aeson
import Web.HttpApiData

-- | User type
newtype PlainPassword = PlainPassword Text deriving (Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Eq)
newtype HashedPassword = HashedPassword {unHashed :: ByteString}
data NoPassword = NoPassword

newtype Email = Email { unEmail :: Text } deriving (Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

newtype Username = Username { unUsername :: Text } deriving (Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

instance SqlType HashedPassword where
  mkLit = LCustom . LBlob . unHashed
  fromSql (SqlBlob x) = HashedPassword x
  fromSql _ = error "fromSql: Bad hash"
  defaultValue = mkLit (HashedPassword "") -- Makes no sense

instance SqlType Email where
  mkLit = LCustom . LText . unEmail
  fromSql (SqlString x) = Email x
  fromSql _ = error "fromSql: Bad email"
  defaultValue = mkLit (Email "")

instance SqlType Username where
  mkLit = LCustom . LText . unUsername
  fromSql (SqlString x) = Username x
  fromSql _ = error "fromSql: Bad username"
  defaultValue = mkLit (Username "")

newtype UserID = UserID {unUserID :: Int} deriving (Show)

newtype BookID = BookID {unBookID :: Int} deriving (Show, ToJSON, FromJSON, FromHttpApiData, Eq, Ord)

newtype ChannelID = ChannelID {unChannelID :: Int} deriving (Show)

newtype TagID = TagID {unTagID :: Int} deriving (Show)

instance SqlType UserID where
  mkLit = LCustom . LInt . unUserID
  fromSql (SqlInt x) = UserID x
  fromSql _ = error "fromSql: Bad userid"
  sqlType _ = TRowID
  defaultValue = mkLit (UserID (-1))
instance SqlType BookID where
  mkLit = LCustom . LInt . unBookID
  fromSql (SqlInt x) = BookID x
  fromSql _ = error "fromSql: Bad bookid"
  defaultValue = mkLit (BookID (-1))
instance SqlType ChannelID where
  mkLit = LCustom . LInt . unChannelID
  fromSql (SqlInt x) = ChannelID x
  fromSql _ = error "fromSql: Bad channelid"
  defaultValue = mkLit (ChannelID (-1))
instance SqlType TagID where
  mkLit = LCustom . LInt . unTagID
  fromSql (SqlInt x) = TagID x
  fromSql _ = error "fromSql: Bad tagid"
  defaultValue = mkLit (TagID (-1))

data User pass = User { identifier :: UserID
                      , email :: Email
                      , username :: Username
                      , role :: Role
                      , password :: pass }
          deriving (Show, Generic)

data Role = UserRole | AdminRole deriving (Show, Read, Enum, Bounded, Typeable, Generic)

instance ToJSON Role
instance FromJSON Role

instance SqlType Role where
  mkLit = LCustom . LText . pack . show
  fromSql sql = case sql of
                     SqlString x -> fromMaybe (error "fromSql: Not a valid role") . readMay . unpack $ x
                     _ -> error "fromSql: Not a valid role"

  defaultValue = mkLit minBound

users :: GenTable (User HashedPassword)
users = genTable "users" [ (email :: User HashedPassword -> Email) :- uniqueGen
                         , username :- uniqueGen
                         , (identifier :: User HashedPassword -> UserID) :- autoPrimaryGen ]

-- | Book type
newtype HashDigest = HashDigest { unHex :: ByteString } deriving Show
-- XXX: Add an identifier for the book
data Book = Book { identifier :: BookID
                 , contentHash :: Maybe HashDigest
                 , contentType :: Text
                 , title :: Maybe Text
                 , description :: Maybe Text
                 , owner :: UserID }
          deriving (Show, Generic)

instance SqlType HashDigest where
  mkLit = LCustom . LBlob . unHex
  fromSql (SqlBlob x) = HashDigest x
  fromSql _ = error "fromSql: Not a valid hash digest"
  defaultValue = mkLit (HashDigest "") -- Doesn't really make sense

books :: GenTable Book
books = genTable "books" [ (identifier :: Book -> BookID) :- autoPrimaryGen
                         , (owner :: Book -> UserID) :- fkGen (gen users) userId ]
  where
    userId :*: _ = selectors (gen users)

-- | Categorizing books
data Tag = Tag { identifier :: TagID
               , tag :: Text
               , owner :: UserID }
         deriving (Show, Generic)

data Channel = Channel { identifier :: ChannelID
                       , channel :: Text
                       , owner :: UserID }
             deriving (Show, Generic)

tags :: GenTable Tag
tags = genTable "tags" [ (identifier :: Tag -> TagID) :- autoPrimaryGen
                       , (owner :: Tag -> UserID) :- fkGen (gen users) i ]
  where
    i :*: _ = selectors (gen users)

channels :: GenTable Channel
channels = genTable "channels" [ (identifier :: Channel -> ChannelID) :- autoPrimaryGen
                               , (owner :: Channel -> UserID) :- fkGen (gen users) i ]
  where
    i :*: _ = selectors (gen users)

data BookTag = BookTag { tag :: TagID
                       , book :: BookID }
             deriving (Show, Generic)

data BookChannel = BookChannel { channel :: ChannelID
                               , book :: BookID }
                 deriving (Show, Generic)

bookTags :: GenTable BookTag
bookTags = genTable "book_tags" [ (tag :: BookTag -> TagID) :- fkGen (gen tags) i
                                , (book :: BookTag -> BookID) :- fkGen (gen books) h ]
  where
    i :*: _ = selectors (gen tags)
    h :*: _ = selectors (gen books)

bookChannels :: GenTable BookChannel
bookChannels = genTable "book_channels" [ (channel :: BookChannel -> ChannelID) :- fkGen (gen channels) i
                                        , (book :: BookChannel -> BookID) :- fkGen (gen books) h ]
  where
    i :*: _ = selectors (gen channels)
    h :*: _ = selectors (gen books)
