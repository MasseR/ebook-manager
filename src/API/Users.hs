{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeOperators #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeApplications #-}
module API.Users  where

import Servant
import ClassyPrelude
import Types
import Data.Aeson
import Web.FormUrlEncoded
import Database (runDB)
import Database.User
import Database.Schema
import Control.Lens (view)
import Data.Generics.Product
import Servant.Auth as SA
import Servant.Auth.Server as SAS


data RegisterForm = RegisterForm { username :: Username
                                 , email :: Email
                                 , password :: PlainPassword
                                 , passwordAgain :: PlainPassword }
                  deriving (Generic, Show)

data LoginStatus = LoginStatus ( Maybe SafeUser ) deriving Generic

data RegisterStatus = RegisterStatus deriving Generic

instance ToJSON LoginStatus
instance FromJSON LoginStatus

instance FromJSON RegisterForm
instance ToJSON RegisterForm
instance ToJSON RegisterStatus
instance FromJSON RegisterStatus
instance FromForm RegisterForm
instance ToForm RegisterForm

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

authCheck :: App -> BasicAuthData -> IO (AuthResult SafeUser)
authCheck app (BasicAuthData username password) = flip runReaderT app $
  maybe SAS.Indefinite authenticated <$> runDB (validateUser username' password')
  where
    username' = Username $ decodeUtf8 username
    password' = PlainPassword $ decodeUtf8 password
    authenticated = SAS.Authenticated . view (super @SafeUser)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult SafeUser)

instance FromBasicAuthData SafeUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

type API = Auth '[SA.BasicAuth] SafeUser :> "login" :> Get '[JSON] LoginStatus
      :<|> "register" :> ReqBody '[JSON, FormUrlEncoded] RegisterForm :> Post '[JSON] RegisterStatus

handler :: ServerT API AppM
handler = loginHandler :<|> registerHandler

loginHandler :: AuthResult SafeUser -> AppM LoginStatus
loginHandler (Authenticated u) = return (LoginStatus (Just u))
loginHandler _ = return (LoginStatus Nothing)

registerHandler :: RegisterForm -> AppM RegisterStatus
registerHandler RegisterForm{..} =
  case () of
       () | password /= passwordAgain -> noMatch
          | otherwise ->
              either (const alreadyExists) (const (pure RegisterStatus)) =<< runDB (insertUser username email password)
  where
    noMatch = throwM err403{errBody = "passwords don't match"}
    alreadyExists = throwM err403{errBody = "User already exists"}
