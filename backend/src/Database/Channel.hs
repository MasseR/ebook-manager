{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language NamedFieldPuns #-}
module Database.Channel
  ( userChannels
  , insertChannel
  , channelExists
  , isChannelOwner
  , updateChannelPrivacy
  , attachChannel
  , Visibility(..)
  , clearChannels
  , booksChannels
  , channelBooks
  , Channel(..)
  , ChannelID(..) )
  where

import ClassyPrelude
import Control.Monad.Catch (MonadMask)
import Database
import Database.Schema
import Database.Selda
import Database.Selda.Generic

import Control.Monad.Trans.Maybe

getChannel :: (MonadSelda m, MonadIO m) => ChannelID -> m (Maybe Channel)
getChannel identifier = listToMaybe . fromRels <$> query q
  where
    q = do
      ch@(channelId :*: _) <- select (gen channels)
      restrict (channelId .== literal identifier)
      return ch

channelExists :: (MonadSelda m, MonadIO m) => ChannelID -> m Bool
channelExists identifier = not . null <$> getChannel identifier

isChannelOwner :: (MonadSelda m, MonadIO m) => ChannelID -> Username -> m Bool
isChannelOwner identifier username = not . null <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      channelId :*: _ :*: channelOwner :*: _ <- select (gen channels)
      restrict (userId .== channelOwner)
      restrict (username' .== literal username)
      restrict (channelId .== literal identifier)
      return channelId

userChannels :: (MonadMask m, MonadIO m) => Username -> SeldaT m [Channel]
userChannels username = fromRels <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      channel@(_ :*: _ :*: owner :*: _) <- select (gen channels)
      restrict (owner .== userId)
      restrict (username' .== literal username)
      return channel

updateChannelPrivacy :: (MonadIO m, MonadSelda m) => ChannelID -> Visibility -> m (Maybe Channel)
updateChannelPrivacy channelId visibility = do
  void $ update (gen channels) predicate (\channel -> channel `with` [pVis := literal visibility])
  getChannel channelId
  where
    predicate (channelId' :*: _) = channelId' .== literal channelId
    _ :*: _ :*: _ :*: pVis = selectors (gen channels)

insertChannel :: (MonadMask m, MonadIO m, MonadSelda m) => Username -> Text -> Visibility -> m (Maybe Channel)
insertChannel username channel visibility = runMaybeT $ do
  userId <- MaybeT (listToMaybe <$> getUser)
  channelId <- toChannelId <$> MaybeT (insertUnless (gen channels) (doesNotExist userId) [ def :*: channel :*: userId :*: visibility ])
  MaybeT (listToMaybe . fromRels <$> query (q channelId))
  where
    q channelId = do
      ch@(channelId' :*: _) <- select (gen channels)
      restrict (channelId' .== literal channelId)
      return ch
    toChannelId = ChannelID . fromRowId
    doesNotExist userId (_ :*: channel' :*: userId' :*: _) = channel' .== literal channel .&& userId' .== literal userId
    getUser = query $ do
      userId :*: _ :*: user :*: _ <- select (gen users)
      restrict (user .== literal username)
      return userId

channelBooks :: (MonadSelda m, MonadIO m) => Username -> ChannelID -> m [Book]
channelBooks username identifier = fromRels <$> query q
  where
    q = do
      channelId :*: bookId' <- select (gen bookChannels)
      channelId' :*: _ :*: owner :*: _ <- select (gen channels)
      userId :*: _ :*: username' :*: _ <- select (gen users)
      book@(bookId :*: _) <- select (gen books)
      restrict (username' .== literal username .&& owner .== userId)
      restrict (channelId .== literal identifier .&& channelId .== channelId')
      restrict (bookId .== bookId')
      return book

booksChannels :: (MonadSelda m, MonadIO m) => BookID -> m [Channel]
booksChannels bookId = fromRels <$> query q
  where
    q = do
      channelId :*: bookId' <- select (gen bookChannels)
      ch@(channelId' :*: _) <- select (gen channels)
      restrict (channelId .== channelId')
      restrict (bookId' .== literal bookId)
      return ch

attachChannel :: (MonadIO m, MonadSelda m) => Username -> BookID -> Text -> m ()
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

clearChannels :: (MonadIO m, MonadSelda m) => BookID -> m Int
clearChannels bookId = deleteFrom (gen bookChannels) (\(_ :*: bookId') -> bookId' .== literal bookId)
