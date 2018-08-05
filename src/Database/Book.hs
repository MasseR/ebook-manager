{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module Database.Book (usersBooks, Book(..)) where

import ClassyPrelude
import Database.Schema
import Database
import Database.Selda

usersBooks :: (MonadMask m, MonadIO m) => Username -> SeldaT m [Book]
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

