{-# Language LambdaCase #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language TemplateHaskell #-}
module Database.User where

import ClassyPrelude
import Control.Lens (view, over, _Just)
import Control.Monad (mfilter)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Crypto.KDF.BCrypt
import Crypto.Random.Types (MonadRandom)
import Data.Generics.Product
import Database
import Database.Schema
import Database.Selda

data UserExistsError = UserExistsError


insertUser :: (MonadMask m, MonadLogger m, MonadIO m, MonadRandom m) => Username -> Email -> PlainPassword -> SeldaT m (Either UserExistsError (User NoPassword))
insertUser username email (PlainPassword password) =
  getUser' username >>= maybe insert' (const (return $ Left UserExistsError))
  where
    insert' = adminExists >>= \e -> Right <$> if e then insertAs UserRole else insertAs AdminRole
    insertAs role = do
      lift $ $logInfo $ "Inserting new user as " <> pack (show role)
      let bytePass = encodeUtf8 password
      user <- User def email username role . HashedPassword <$> lift (hashPassword 12 bytePass)
      insert_ (gen users) [toRel user] >> return (over (field @"password") (const NoPassword) user)

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

getUser :: (MonadMask m, MonadIO m) => Username -> SeldaT m (Maybe (User NoPassword))
getUser name = over (_Just . field @"password") (const NoPassword) <$> getUser' name

validateUser :: (MonadMask m, MonadIO m) => Username -> PlainPassword -> SeldaT m (Maybe (User NoPassword))
validateUser name (PlainPassword password) =
  asHidden . mfilter valid <$> getUser' name
  where
    valid = validatePassword password' . unHashed . view (field @"password")
    password' = encodeUtf8 password
    asHidden = over (_Just . field @"password") (const NoPassword)

getUser' :: (MonadMask m, MonadIO m) => Username -> SeldaT m (Maybe ( User HashedPassword ))
getUser' name = listToMaybe . fmap fromRel <$> query q
  where
    q = do
      u@(_ :*: _ :*: username :*: _ ) <- select (gen users)
      restrict (username .== literal name)
      return u
