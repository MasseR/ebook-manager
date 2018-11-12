{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeOperators #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeApplications #-}
module API.Users  where

import           ClassyPrelude
import           Control.Monad.Catch (throwM, MonadThrow)
import           Data.Aeson
import           Database (runDB)
import           Database.Schema
import           Database.User
import           Servant
import           Servant.Auth as SA
import           Servant.Auth.Server as SAS
import qualified Servant.Docs as Docs
import           Server.Auth
import           Types
import           Web.FormUrlEncoded


data RegisterForm = RegisterForm { username :: Username
                                 , email :: Email
                                 , password :: PlainPassword
                                 , passwordAgain :: PlainPassword }
                  deriving (Generic, Show)

instance Docs.ToSample RegisterForm

data LoginStatus = LoginStatus ( Maybe SafeUser ) deriving Generic

data RegisterStatus = RegisterStatus deriving Generic

instance Docs.ToSample RegisterStatus

instance ToJSON LoginStatus
instance FromJSON LoginStatus
instance Docs.ToSample LoginStatus

instance FromJSON RegisterForm
instance ToJSON RegisterForm
instance ToJSON RegisterStatus
instance FromJSON RegisterStatus
instance FromForm RegisterForm
instance ToForm RegisterForm


type API = Auth '[SA.BasicAuth, SA.JWT] SafeUser :> "login" :> Get '[JSON] LoginStatus
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
