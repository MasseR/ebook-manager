{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeOperators #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeApplications #-}
{-# Language TemplateHaskell #-}
module Server.Auth
  ( SafeUser(..)
  , authCheck
  , AuthResult(..)
  , requireLoggedIn)
  where

import ClassyPrelude
import Control.Lens (view)
import Control.Monad.Logger
import Control.Monad.Catch (throwM, MonadThrow)
import Data.Aeson
import Data.Generics.Product
import Database
import Database.Schema
import Database.User
import Servant (err401)
import Servant.Auth.Server as SAS
import Types

-- generic-lens can convert similar types to this
-- I'm trying out servant-auth-server which uses a jwt style login. IIRC anyone
-- can open the jwt token and view what's inside, you just can't modify it.
--
-- Is it a problem that a human readable username and email are visible?
data SafeUser = SafeUser { email :: Email
                         , username :: Username
                         , role :: Role }
              deriving (Show, Generic)

instance ToJSON SafeUser where
instance FromJSON SafeUser where
instance ToJWT SafeUser where
instance FromJWT SafeUser where

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult SafeUser)

instance FromBasicAuthData SafeUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

authCheck :: App -> BasicAuthData -> IO (AuthResult SafeUser)
authCheck app (BasicAuthData username password) = flip runReaderT app $
  maybe SAS.Indefinite authenticated <$> runDB (validateUser username' password')
  where
    username' = Username $ decodeUtf8 username
    password' = PlainPassword $ decodeUtf8 password
    authenticated = SAS.Authenticated . view (super @SafeUser)

requireLoggedIn :: (MonadThrow m, MonadLogger m, Monad m) => (SafeUser -> m a) -> AuthResult SafeUser -> m a
requireLoggedIn f (Authenticated user) = f user
requireLoggedIn _ u = $logError (pack (show u)) >> throwM err401
