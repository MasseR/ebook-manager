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

data LoginForm = LoginForm { username :: Text
                           , password :: Text }
               deriving (Generic, Show)

data RegisterForm = RegisterForm { username :: Username
                                 , email :: Email
                                 , password :: PlainPassword
                                 , passwordAgain :: PlainPassword }
                  deriving (Generic, Show)

data LoginStatus = LoginStatus ( Maybe SafeUser ) deriving Generic

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

-- generic-lens can convert similar types to this
data SafeUser = SafeUser { email :: Email
                         , username :: Username
                         , role :: Role }
              deriving (Show, Generic)

instance ToJSON SafeUser where
instance FromJSON SafeUser where

type API = "login" :> ReqBody '[JSON, FormUrlEncoded] LoginForm :> Post '[JSON] LoginStatus
      :<|> "register" :> ReqBody '[JSON, FormUrlEncoded] RegisterForm :> Post '[JSON] RegisterStatus

handler :: ServerT API AppM
handler = loginHandler :<|> registerHandler

loginHandler :: LoginForm -> AppM LoginStatus
loginHandler LoginForm{..} = do
  user <- fmap (view (super @SafeUser)) <$> runDB (validateUser (Username username) (PlainPassword password))
  return (LoginStatus user)

registerHandler :: RegisterForm -> AppM RegisterStatus
registerHandler RegisterForm{..} =
  case () of
       () | password /= passwordAgain -> noMatch
          | otherwise ->
              either (const alreadyExists) (const (pure RegisterStatus)) =<< runDB (insertUser username email password)
  where
    noMatch = throwM err403{errBody = "passwords don't match"}
    alreadyExists = throwM err403{errBody = "User already exists"}
