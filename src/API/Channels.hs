{-# Language DataKinds #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language NoImplicitPrelude #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
module API.Channels (API, handler) where

import Servant
import Types
import ClassyPrelude
import Server.Auth
import Servant.Auth as SA
import Control.Monad.Logger
import Database
import Database.Channel
import Data.Aeson
import Control.Lens
import Data.Generics.Product

data JsonChannel = JsonChannel { channel :: Text } deriving (Show, Generic)

instance ToJSON JsonChannel
instance FromJSON JsonChannel

type API = Auth '[SA.BasicAuth, SA.Cookie, SA.JWT] SafeUser :> BaseAPI

type BaseAPI = "channels" :> ReqBody '[JSON] JsonChannel :> Put '[JSON] JsonChannel
          :<|> "channels" :> Get '[JSON] [JsonChannel]

handler :: ServerT API AppM
handler user = newChannelHandler user :<|> listChannelsHandler user

listChannelsHandler :: AuthResult SafeUser -> AppM [JsonChannel]
listChannelsHandler = requireLoggedIn $ \user ->
  -- I could use the super thing from generic-lens, but then I would need to
  -- use the 'channel' accessor somehow or export it
  fmap (\Channel{..} -> JsonChannel{..}) <$> runDB (userChannels (view (field @"username") user))

newChannelHandler :: AuthResult SafeUser -> JsonChannel -> AppM JsonChannel
newChannelHandler auth ch@JsonChannel{..} = flip requireLoggedIn auth $ \user -> do
  $logInfo $ "Creating channel for user " <> pack (show user)
  runDB (insertChannel (view (field @"username") user) channel)
  return ch
