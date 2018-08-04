{-# Language NoImplicitPrelude #-}
{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}
module Database.Schema where

import ClassyPrelude
import Database.Selda.Generic
import Database.Selda
import Database.Selda.Backend

-- | User type
data User pass = User { identifier :: RowID
                      , email :: Text
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
users = genTable "users" [ (email :: User ByteString -> Text) :- uniqueGen
                         , username :- uniqueGen
                         , (identifier :: User ByteString -> RowID) :- autoPrimaryGen ]

-- | Book type
newtype HashDigest = HashDigest { unHex :: Text } deriving Show
data Book = Book { contentHash :: HashDigest
                 , contentType :: Text
                 , title :: Maybe Text
                 , description :: Maybe Text }
          deriving (Show, Generic)

instance SqlType HashDigest where
  mkLit = LCustom . LText . unHex
  fromSql (SqlString x) = HashDigest x
  fromSql _ = error "fromSql: Not a valid hash digest"
  defaultValue = mkLit (HashDigest "") -- Doesn't really make sense

books :: GenTable Book
books = genTable "books" [ contentHash :- primaryGen ]

data UserBook = UserBook { email :: Text
                         , book :: HashDigest }
              deriving (Generic, Show)

userBooks :: GenTable UserBook
userBooks = genTable "user_book" [ (email :: UserBook -> Text) :- fkGen (gen users) userEmail
                                 , (book :: UserBook -> HashDigest) :- fkGen (gen books) bookHash ]
  where
    _ :*: userEmail :*: _ = selectors (gen users)
    bookHash :*: _ = selectors (gen books)

-- | Categorizing books
data Tag = Tag { identifier :: RowID
               , tag :: Text
               , owner :: RowID }
         deriving (Show, Generic)

data Channel = Channel { identifier :: RowID
                       , channel :: Text
                       , owner :: RowID }
             deriving (Show, Generic)

tags :: GenTable Tag
tags = genTable "tags" [ (identifier :: Tag -> RowID) :- autoPrimaryGen
                       , (owner :: Tag -> RowID) :- fkGen (gen users) i ]
  where
    i :*: _ = selectors (gen users)

channels :: GenTable Channel
channels = genTable "tags" [ (identifier :: Channel -> RowID) :- autoPrimaryGen
                           , (owner :: Channel -> RowID) :- fkGen (gen users) i ]
  where
    i :*: _ = selectors (gen users)

data BookTag = BookTag { tag :: RowID
                       , book :: HashDigest }
             deriving (Show, Generic)

data BookChannel = BookChannel { channel :: RowID
                               , book :: HashDigest }
                 deriving (Show, Generic)

bookTags :: GenTable BookTag
bookTags = genTable "book_tags" [ (tag :: BookTag -> RowID) :- fkGen (gen tags) i
                                , (book :: BookTag -> HashDigest) :- fkGen (gen books) h ]
  where
    i :*: _ = selectors (gen tags)
    h :*: _ = selectors (gen books)

bookChannels :: GenTable BookChannel
bookChannels = genTable "book_channels" [ (channel :: BookChannel -> RowID) :- fkGen (gen channels) i
                                        , (book :: BookChannel -> HashDigest) :- fkGen (gen books) h ]
  where
    i :*: _ = selectors (gen channels)
    h :*: _ = selectors (gen books)