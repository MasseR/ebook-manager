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
{-# Language DuplicateRecordFields #-}
{-# Language NamedFieldPuns #-}
module API.Channels (API, handler, JsonChannel(..)) where

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

data JsonChannel = JsonChannel { channel :: Text
                               , visibility :: Visibility }
                 deriving (Show, Generic)
data UpdateChannel = UpdateChannel { identifier :: ChannelID
                                   , channel :: Text
                                   , visibility :: Visibility }
                 deriving (Show, Generic)

instance ToJSON JsonChannel
instance FromJSON JsonChannel
instance ToJSON UpdateChannel
instance FromJSON UpdateChannel

type API = Auth '[SA.BasicAuth, SA.Cookie, SA.JWT] SafeUser :> BaseAPI

type BaseAPI = "channels" :> ReqBody '[JSON] JsonChannel :> Post '[JSON] UpdateChannel
          :<|> "channels" :> Capture "channel_id" ChannelID :> ReqBody '[JSON] UpdateChannel :> Put '[JSON] UpdateChannel
          :<|> "channels" :> Get '[JSON] [JsonChannel]

handler :: ServerT API AppM
handler user = newChannelHandler user :<|> updateChannelHandler user :<|> listChannelsHandler user

requireChannelOwner :: AuthResult SafeUser -> ChannelID -> (SafeUser -> AppM a) -> AppM a
requireChannelOwner auth channelId f = flip requireLoggedIn auth $ \u@SafeUser{username} -> do
  unlessM (runDB . channelExists $ channelId) $ throwM err404
  runDB (isChannelOwner channelId username) >>= \o -> if o then f u else throwM err403

updateChannelHandler :: AuthResult SafeUser -> ChannelID -> UpdateChannel -> AppM UpdateChannel
updateChannelHandler auth channelId UpdateChannel{visibility} = requireChannelOwner auth channelId $ \_ -> do
  mChannel <- fmap toChannel <$> runDB (updateChannelPrivacy channelId visibility)
  maybe (throwM err403) return mChannel

listChannelsHandler :: AuthResult SafeUser -> AppM [JsonChannel]
listChannelsHandler = requireLoggedIn $ \user ->
  -- I could use the super thing from generic-lens, but then I would need to
  -- use the 'channel' accessor somehow or export it
  fmap (\Channel{..} -> JsonChannel{..}) <$> runDB (userChannels (view (field @"username") user))

newChannelHandler :: AuthResult SafeUser -> JsonChannel -> AppM UpdateChannel
newChannelHandler auth JsonChannel{..} = flip requireLoggedIn auth $ \user -> do
  $logInfo $ "Creating channel for user " <> pack (show user)
  mChannel <- fmap toChannel <$> runDB (insertChannel (view (field @"username") user) channel visibility)
  maybe (throwM err403{errBody="Could not create the channel"}) return mChannel

toChannel :: Channel -> UpdateChannel
toChannel Channel{..} = UpdateChannel{..}
