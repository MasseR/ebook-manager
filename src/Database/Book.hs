{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module Database.Book
  ( def
  , insertBook
  , InsertBook(..)
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
