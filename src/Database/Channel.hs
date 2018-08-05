{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module Database.Channel
  ( userChannels
  , insertChannel
  , Channel(..) )
  where

import ClassyPrelude
import Database.Schema
import Database
import Database.Selda

userChannels :: (MonadMask m, MonadIO m) => Username -> SeldaT m [Channel]
userChannels username = fromRels <$> query q
  where
    q = do
      userId :*: _ :*: username' :*: _ <- select (gen users)
      channel@(_ :*: _ :*: owner) <- select (gen channels)
      restrict (owner .== userId)
      restrict (username' .== literal username)
      return channel

insertChannel :: (MonadMask m, MonadIO m) => Username -> Text -> SeldaT m ()
insertChannel username channel = do
  mUserId <- listToMaybe <$> getUser
  void $ forM mUserId $ \userId ->
    insertUnless (gen channels) (doesNotExist userId) [ def :*: channel :*: userId ]
  where
    doesNotExist userId (_ :*: channel' :*: userId') = channel' .== literal channel .&& userId' .== literal userId
    getUser = query $ do
      userId :*: _ :*: user :*: _ <- select (gen users)
      restrict (user .== literal username)
      return userId
