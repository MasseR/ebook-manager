{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module API.Channels (API, handler, JsonChannel(..)) where

import           ClassyPrelude
import           Control.Lens
import           Control.Monad.Catch   (MonadThrow, throwM)
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Generics.Product
import           Database
import           Database.Channel
import           Servant
import           Servant.Auth          as SA
import qualified Servant.Docs          as Docs
import           Server.Auth
import           Types

data JsonChannel = JsonChannel { channel    :: Text
                               , visibility :: Visibility }
                 deriving (Show, Generic)
data UpdateChannel = UpdateChannel { identifier :: ChannelID
                                   , channel    :: Text
                                   , visibility :: Visibility }
                 deriving (Show, Generic)

instance Docs.ToSample JsonChannel where
  toSamples _ = [("Channel", JsonChannel "channel" Private)]

instance Docs.ToSample UpdateChannel where
  toSamples _ = [("Channel", UpdateChannel 13 "channel" Private)]

instance ToJSON JsonChannel
instance FromJSON JsonChannel
instance ToJSON UpdateChannel
instance FromJSON UpdateChannel

type API = Auth '[SA.BasicAuth, SA.Cookie, SA.JWT] SafeUser :> BaseAPI

instance Docs.ToCapture (Capture "channel_id" ChannelID) where
  toCapture _ = Docs.DocCapture "channel_id" "The channel id"

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
