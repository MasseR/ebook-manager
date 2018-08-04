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
import Server.Auth
import Servant.Auth.Server as SAS
import Servant.Auth as SA


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
