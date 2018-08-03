{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language OverloadedStrings #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeOperators #-}
{-# Language DuplicateRecordFields #-}
module API.Users (API, handler) where

import Servant
import ClassyPrelude
import Types
import Data.Aeson
import Web.FormUrlEncoded
import Database (runDB)
import Database.User

data LoginForm = LoginForm { username :: Text
                           , password :: Text }
               deriving (Generic, Show)

data RegisterForm = RegisterForm { username :: Text
                                 , email :: Text
                                 , password :: Text
                                 , passwordAgain :: Text }
                  deriving (Generic, Show)

data LoginStatus = LoginStatus deriving Generic

data RegisterStatus = RegisterStatus deriving Generic

instance FromJSON LoginForm
instance ToJSON LoginForm
instance ToJSON LoginStatus
instance FromJSON LoginStatus
instance FromForm LoginForm
instance ToForm LoginForm

instance FromJSON RegisterForm
instance ToJSON RegisterForm
instance ToJSON RegisterStatus
instance FromJSON RegisterStatus
instance FromForm RegisterForm
instance ToForm RegisterForm

type API = "login" :> ReqBody '[JSON, FormUrlEncoded] LoginForm :> Post '[JSON] LoginStatus
      :<|> "register" :> ReqBody '[JSON, FormUrlEncoded] RegisterForm :> Post '[JSON] RegisterStatus

handler :: ServerT API AppM
handler = loginHandler :<|> registerHandler

loginHandler :: LoginForm -> AppM LoginStatus
loginHandler LoginForm{..} = throwM err403

registerHandler :: RegisterForm -> AppM RegisterStatus
registerHandler RegisterForm{..} =
  case () of
       () | password /= passwordAgain -> noMatch
          | otherwise ->
              either (const alreadyExists) (const (pure RegisterStatus)) =<< runDB (insertUser username email (PlainPassword password))
  where
    noMatch = throwM err403{errBody = "passwords don't match"}
    alreadyExists = throwM err403{errBody = "User already exists"}
