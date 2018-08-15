{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language NamedFieldPuns #-}
module Database.Channel
  ( userChannels
  , insertChannel
  , attachChannel
  , Visibility(..)
  , clearChannels
  , booksChannels
  , Channel(..)
  , ChannelID )
  where

import ClassyPrelude
import Database.Schema
import Database
import Database.Selda
import Database.Selda.Generic

userChannels :: (MonadMask m, MonadIO m) => Username -> SeldaT m [Channel]
userChannels username = fromRels <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      channel@(_ :*: _ :*: owner :*: _) <- select (gen channels)
      restrict (owner .== userId)
      restrict (username' .== literal username)
      return channel

insertChannel :: (MonadMask m, MonadIO m) => Username -> Text -> Visibility -> SeldaT m ()
insertChannel username channel visibility = do
  mUserId <- listToMaybe <$> getUser
  void $ forM mUserId $ \userId ->
    insertUnless (gen channels) (doesNotExist userId) [ def :*: channel :*: userId :*: visibility ]
  where
    doesNotExist userId (_ :*: channel' :*: userId' :*: _) = channel' .== literal channel .&& userId' .== literal userId
    getUser = query $ do
      userId :*: _ :*: user :*: _ <- select (gen users)
      restrict (user .== literal username)
      return userId

booksChannels :: (MonadSelda m, MonadMask m, MonadIO m) => BookID -> m [Channel]
booksChannels bookId = fromRels <$> query q
  where
    q = do
      channelId :*: bookId' <- select (gen bookChannels)
      ch@(channelId' :*: _) <- select (gen channels)
      restrict (channelId .== channelId')
      restrict (bookId' .== literal bookId)
      return ch

attachChannel :: (MonadMask m, MonadIO m, MonadSelda m) => Username -> BookID -> Text -> m ()
attachChannel username bookId channel = do
  mCh <- fromRels <$> query channelQ
  forM_ mCh $ \Channel{identifier} ->
    whenM (null <$> query (attachQ identifier)) $
      void $ insertGen bookChannels [BookChannel identifier bookId]
  where
    attachQ channelId = do
      (channelId' :*: bookId') <- select (gen bookChannels)
      restrict (channelId' .== literal channelId .&& bookId' .== literal bookId)
      return channelId'
    channelQ = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      ch@(_ :*: channel' :*: owner :*: _) <- select (gen channels)
      restrict (username' .== literal username)
      restrict (owner .== userId)
      restrict (channel' .== literal channel)
      return ch

clearChannels :: (MonadMask m, MonadIO m, MonadSelda m) => BookID -> m Int
clearChannels bookId = deleteFrom (gen bookChannels) (\(_ :*: bookId') -> bookId' .== literal bookId)
