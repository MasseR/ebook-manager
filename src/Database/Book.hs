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

import Database.Tag (attachTag)

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
                             , tags :: [Text]}

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
updateBook book@UpdateBook{..} = do
  connectChannels
  connectTags
  updateBook'
  where
    connectTags = mapM_ (attachTag owner identifier) tags
    connectChannels = return ()
    updateBook' = do
      mUserId <- query (bookOwner' identifier owner)
      forM (listToMaybe mUserId) $ \_userId -> do
        update_ (gen books) predicate (\b -> b `with` [ pContentType := literal contentType
                                                      , pTitle := literal title
                                                      , pDescription := literal description ])
        return book
    _ :*: _ :*: pContentType :*: pTitle :*: pDescription :*: _ = selectors (gen books)
    predicate (bookId :*: _) = bookId .== literal identifier

setContent :: (MonadSelda m, MonadMask m, MonadIO m) => BookID -> Username -> HashDigest -> m ()
setContent identifier owner digest = do
  mOwner <- query (bookOwner' identifier owner)
  void $ forM (listToMaybe mOwner) $ \_ ->
    update_ (gen books) predicate (\b -> b `with` [ pHash := literal (Just digest)])
  where
    _ :*: pHash :*: _ = selectors (gen books)
    predicate (bookId :*: _) = bookId .== literal identifier
