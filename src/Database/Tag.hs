{-# Language TypeApplications #-}
{-# Language TypeOperators #-}
{-# Language DataKinds #-}
{-# Language DuplicateRecordFields #-}
{-# Language NamedFieldPuns #-}
module Database.Tag
  ( def
  , booksTags
  , attachTag
  , upsertTag
  , clearTags
  , Tag(..) ) where

import ClassyPrelude
import Database.Schema
import Database
import Database.Selda
import Database.Selda.Generic

upsertTag :: (MonadMask m, MonadIO m, MonadSelda m) => Username -> Text -> m Tag
upsertTag username tag = do
  -- I want this to error out if some data is invariant is wrong and roll back
  -- the transaction. Also as a side note, run this in a transaction plz
  [userId] <- query userQ
  void $ upsert (gen tags) (predicate userId) id [toRel (Tag def tag userId)]
  [t] <- fromRels <$> query (tagQ userId)
  return t
  where
    predicate userId (_ :*: tag' :*: owner) = tag' .== literal tag .&& owner .== literal userId
    tagQ userId = do
      t@(_ :*: tag' :*: owner) <- select (gen tags)
      restrict (tag' .== literal tag .&& owner .== literal userId)
      return t
    userQ = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      restrict (username' .== literal username)
      return userId

booksTags :: (MonadMask m, MonadIO m, MonadSelda m) => BookID -> m [Tag]
booksTags bookId = fromRels <$> query q
  where
    q = do
      tagId :*: bookId' <- select (gen bookTags)
      tag@(tagId' :*: _) <- select (gen tags)
      restrict (tagId .== tagId')
      restrict (bookId' .== literal bookId)
      return tag

attachTag :: (MonadMask m, MonadIO m, MonadSelda m) => Username -> BookID -> Text -> m ()
attachTag username bookId tag = do
  Tag{identifier} <- upsertTag username tag
  whenM (null <$> query (tagQ identifier)) $
    void $ insertGen bookTags [BookTag identifier bookId]
  where
    tagQ tagId = do
      (tagId' :*: bookId') <- select (gen bookTags)
      restrict (tagId' .== literal tagId .&& bookId' .== literal bookId)
      return tagId'

clearTags :: (MonadMask m, MonadIO m, MonadSelda m) => BookID -> m Int
clearTags bookId = deleteFrom (gen bookTags) (\(_ :*: bookId') -> bookId' .== literal bookId)

