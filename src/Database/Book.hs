{-# Language TypeApplications #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
module Database.Book
  ( def
  , insertBook
  , getBook
  , bookExists
  , updateBook
  , isBookOwner
  , setContent
  , InsertBook(..)
  , UpdateBook(..)
  , usersBooks
  , Book(..)
  , HashDigest(..)
  , BookID) where

import ClassyPrelude
import Database.Schema (books, users, Username, Book(..), BookID(..), UserID, HashDigest(..))
import Database
import Database.Selda
import Database.Selda.Generic

import Control.Lens (view)
import Data.Generics.Product

import Database.Tag (booksTags, attachTag, clearTags)
import Database.Channel (booksChannels, attachChannel, clearChannels)

usersBooks :: (MonadSelda m, MonadMask m, MonadIO m) => Username -> m [Book]
usersBooks username = fromRels <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      book@(_ :*: digest :*: _ :*: _ :*: _ :*: owner) <- select (gen books)
      restrict (username' .== literal username)
      restrict (userId .== owner)
      restrict (not_ (isNull digest))
      return book


getBook :: (MonadSelda m, MonadMask m, MonadIO m) => BookID -> Username -> m (Maybe Book)
getBook identifier owner = listToMaybe . fromRels <$> query q
  where
    q = do
      _ :*: bookId <- bookOwner' identifier owner
      book@(bookId' :*: _) <- select (gen books)
      restrict (bookId .== bookId')
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
                             , owner :: Username
                             , tags :: [Text]
                             , channels :: [Text] }
                deriving (Show, Generic)

bookExists :: (MonadSelda m, MonadMask m, MonadIO m) => BookID -> m Bool
bookExists identifier = not . null <$> query q
  where
    q = do
      (bookId :*: _) <- select (gen books)
      restrict (bookId .== literal identifier)
      return bookId

isBookOwner :: (MonadSelda m, MonadIO m, MonadThrow m) => BookID -> Username -> m Bool
isBookOwner identifier username = not . null <$> query (bookOwner' identifier username)

bookOwner' :: BookID -> Username -> Query s (Col s UserID :*: Col s BookID)
bookOwner' identifier username = do
  userId :*: _ :*: username' :*: _ <- select (gen users)
  bookId :*: _ :*: _ :*: _ :*: _ :*: bookOwner <- select (gen books)
  restrict (userId .== bookOwner)
  restrict (username' .== literal username)
  restrict (bookId .== literal identifier)
  return (userId :*: bookId)

updateBook :: (MonadSelda m, MonadMask m, MonadIO m) => UpdateBook -> m (Maybe UpdateBook)
updateBook UpdateBook{..} = do
  clearTags identifier >> connectTags
  clearChannels identifier >> connectChannels
  updateBook'
  getUpdateBook identifier owner
  where
    connectTags = mapM_ (attachTag owner identifier) tags
    connectChannels = mapM_ (attachChannel owner identifier) channels
    updateBook' = do
      mUserId <- query (bookOwner' identifier owner)
      forM_ (listToMaybe mUserId) $ \_userId -> do
        update_ (gen books) predicate (\b -> b `with` [ pContentType := literal contentType
                                                      , pTitle := literal title
                                                      , pDescription := literal description ])
    _ :*: _ :*: pContentType :*: pTitle :*: pDescription :*: _ = selectors (gen books)
    predicate (bookId :*: _) = bookId .== literal identifier


getUpdateBook :: (MonadMask m, MonadIO m, MonadSelda m) => BookID -> Username -> m (Maybe UpdateBook)
getUpdateBook bookId username = do
  mBook <- getBook bookId username
  forM mBook $ \Book{..} -> do
    channels <- map (view (field @"channel")) <$> booksChannels bookId
    tags <- map (view (field @"tag")) <$> booksTags bookId
    return UpdateBook{owner=username,..}

setContent :: (MonadSelda m, MonadMask m, MonadIO m) => BookID -> Username -> HashDigest -> m ()
setContent identifier owner digest = do
  mOwner <- query (bookOwner' identifier owner)
  void $ forM (listToMaybe mOwner) $ \_ ->
    update_ (gen books) predicate (\b -> b `with` [ pHash := literal (Just digest)])
  where
    _ :*: pHash :*: _ = selectors (gen books)
    predicate (bookId :*: _) = bookId .== literal identifier
