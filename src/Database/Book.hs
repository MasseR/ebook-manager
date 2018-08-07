{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module Database.Book
  ( def
  , insertBook
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
      userId' :*: bookHash' <- select (gen userBooks)
      book@(bookHash :*: _) <- select (gen books)
      restrict (bookHash .== bookHash')
      restrict (username' .== literal username)
      restrict (userId .== userId')
      return book

-- Always inserts
insertBook :: (MonadSelda m, MonadMask m, MonadIO m) => Username -> Book -> m (Maybe BookID)
insertBook username book = do
  bookId <- BookID . fromRowId <$> insertGenWithPK books [book]
  mUserId <- query $ do
    userId :*: _ :*: username' :*: _ <- select (gen users)
    restrict (username' .== literal username)
    return userId
  forM (listToMaybe mUserId) $ \userId -> do
    void $ insertGen userBooks [UserBook userId bookId]
    return bookId
