{-# Language LambdaCase #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
module Database.User where

import ClassyPrelude
import Database
import Database.Schema
import Database.Selda
import Control.Lens (over, _Just)
import Data.Generics.Product
import Crypto.KDF.BCrypt
import Crypto.Random.Types (MonadRandom)
import Control.Monad.Logger

data UserExistsError = UserExistsError

newtype PlainPassword = PlainPassword Text
newtype HashedPassword = HashedPassword {unHashed :: ByteString}
data NoPassword = NoPassword

insertUser :: (MonadLogger m, MonadIO m, MonadMask m, MonadRandom m) => Text -> Text -> PlainPassword -> SeldaT m (Either UserExistsError (User NoPassword))
insertUser username email (PlainPassword password) =
  getUser' username >>= maybe insert' (const (return $ Left UserExistsError))
  where
    insert' = adminExists >>= \e -> Right <$> if e then insertAs UserRole else insertAs AdminRole
    insertAs role = do
      lift $ $logInfo $ "Inserting new user as " <> pack (show role)
      let bytePass = encodeUtf8 password
      user <- User def email username role . HashedPassword <$> lift (hashPassword 12 bytePass)
      insert_ (gen users) [toRel (over (field @"password") unHashed user)] >> return (over (field @"password") (const NoPassword) user)

adminExists :: (MonadMask m, MonadLogger m, MonadIO m) => SeldaT m Bool
adminExists = do
  r <- query q
  lift $ $logInfo $ "Admin users: " <> (pack (show r))
  return $ maybe False (> 0) . listToMaybe $ r
  where
    q = aggregate $ do
      (_ :*: _ :*: _ :*: r :*: _) <- select (gen users)
      restrict (r .== literal AdminRole)
      return (count r)

getUser :: (MonadMask m, MonadIO m) => Text -> SeldaT m (Maybe (User NoPassword))
getUser name = over (_Just . field @"password") (const NoPassword) <$> getUser' name

getUser' :: (MonadMask m, MonadIO m) => Text -> SeldaT m (Maybe ( User HashedPassword ))
getUser' name = over (_Just . field @"password") HashedPassword . listToMaybe . fmap fromRel <$> query q
  where
    q = do
      u@(_ :*: username :*: _ :*: _ :*: _) <- select (gen users)
      restrict (username .== literal name)
      return u
