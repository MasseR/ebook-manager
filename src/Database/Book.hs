{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
module Database.Book
  ( def
  , insertBook
  , updateBook
  , InsertBook(..)
  , UpdateBook(..)
  , usersBooks
  , Book(..)
  , BookID) where

import ClassyPrelude
import Database.Schema
import Database
import Database.Selda
import Database.Selda.Generic

usersBooks :: (MonadSelda m, MonadMask m, MonadIO m) => Username -> m [Book]
usersBooks username = fromRels <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      book@(_ :*: _ :*: _ :*: _ :*: _ :*: owner) <- select (gen books)
      restrict (username' .== literal username)
      restrict (userId .== owner)
      return book

data InsertBook = InsertBook { contentType :: Text
                             , title :: Maybe Text
                             , description :: Maybe Text
                             , owner :: Username }

-- Always inserts
insertBook :: (MonadSelda m, MonadMask m, MonadIO m) => InsertBook -> m (Maybe BookID)
insertBook InsertBook{..} = do
  mUserId <- query $ do
    userId :*: _ :*: username' :*: _ <- select (gen users)
    restrict (username' .== literal owner)
    return userId
  forM (listToMaybe mUserId) $ \userId -> do
    let book = Book{owner=userId,identifier=def,contentHash=Nothing,..}
    BookID . fromRowId <$> insertGenWithPK books [book]

data UpdateBook = UpdateBook { identifier :: BookID
                             , contentType :: Text
                             , title :: Maybe Text
                             , description :: Maybe Text
                             , owner :: Username }

updateBook :: (MonadSelda m, MonadMask m, MonadIO m) => UpdateBook -> m (Maybe UpdateBook)
updateBook book@UpdateBook{..} = do
  mUserId <- query $ do
    userId :*: _ :*: username :*: _ <- select (gen users)
    bookId :*: _ :*: _ :*: _ :*: _ :*: bookOwner <- select (gen books)
    restrict (userId .== bookOwner)
    restrict (username .== literal owner)
    restrict (bookId .== literal identifier)
    return userId
  forM (listToMaybe mUserId) $ \_userId -> do
    update_ (gen books) predicate (\b -> b `with` [ pContentType := literal contentType
                                                  , pTitle := literal title
                                                  , pDescription := literal description ])
    return book
  where
    _ :*: _ :*: pContentType :*: pTitle :*: pDescription :*: _ = selectors (gen books)
    predicate (bookId :*: _) = bookId .== literal identifier
