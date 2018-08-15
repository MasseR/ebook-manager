{-# Language DataKinds #-}
{-# Language NamedFieldPuns #-}
{-# Language TypeApplications #-}
{-# Language KindSignatures #-}
{-# Language TypeFamilies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeOperators #-}
{-# Language StandaloneDeriving #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language FlexibleContexts #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
module API.Catalogue (VersionedAPI, handler) where

import Types
import Servant
import ClassyPrelude
import GHC.TypeLits
import Server.Auth
import Servant.Auth as SA
import Servant.XML
import qualified Database.Channel as Channel
import Database

-- This is my first try on going to versioned apis, things might change
-- I think my rule of thumb is that you can add new things as you want, but
-- deleting and modifying warrants a new version

data family Catalog :: Nat -> *

data family Entry :: Nat -> *

newtype Rel = Rel { unRel :: Text } deriving (IsString, Show)

data Pagination = Pagination { previous :: Maybe Rel
                             , next :: Maybe Rel }
                deriving (Show)

newtype SubSection = SubSection Rel deriving (Show)
newtype Acquisition = Acquisition Rel deriving (Show)

data instance Entry 1 = EntryV1 { title :: Text
                                , identifier :: Text
                                , updated :: UTCTime
                                , content :: Text
                                , link :: Either SubSection Acquisition
                                }

data instance Catalog 1 = CatalogV1 { updated :: UTCTime
                                    , self :: Rel
                                    , start :: Rel
                                    , pagination :: Pagination
                                    , entries :: [Entry 1]
                                    }

deriving instance Show (Catalog 1)
deriving instance Show (Entry 1)
deriving instance Generic (Catalog 1)
deriving instance Generic (Entry 1)

instance ToNode SubSection where
  toNode (SubSection rel) = [xml|<link type="application/atom+xml;profile=opds-catalog;kind=acquisition" rel="subsection" href="#{unRel rel}">|]

instance ToNode Acquisition where
  toNode (Acquisition rel) = [xml|<link type="application/epub+zip" rel="http://opds-spec.org/acquisition" href="#{unRel rel}">|]

instance ToNode (Entry 1) where
  toNode EntryV1{..} = [xml|
<entry>
  <title>#{title}
  <id>#{identifier}
  <updated>#{iso8601 updated}
  <content>#{content}
  ^{either toNode toNode link}
  |]

instance ToNode (Catalog 1) where
  toNode CatalogV1{..} = [xml|
<feed xmlns="http://www.w3.org/2005/Atom" xmlns:opds="http://opds-spec.org/2010/catalog">
  <id>#{unRel self}
  <title>Give me a title
  <updated>#{iso8601 updated}
  <link type="application/atom+xml;profile=opds-catalog;kind=navigation" rel="self" href="#{unRel self}">
  <link type="application/atom+xml;profile=opds-catalog;kind=navigation" rel="start" href="#{unRel start}">
  $maybe n <- (next pagination)
    <link type="application/atom+xml;profile=opds-catalog;kind=navigation" rel="next" href="#{unRel n}">
  $maybe p <- (previous pagination)
    <link type="application/atom+xml;profile=opds-catalog;kind=navigation" rel="previous" href="#{unRel p}">

  ^{toNode entries}
  |]

class Monad m => VersionedCatalog m (v :: Nat) where
  getChannels :: SafeUser -> m (Catalog v)

instance VersionedCatalog AppM 1 where
  getChannels SafeUser{username} = do
    updated <- liftIO getCurrentTime
    let self = Rel ("/api/current/" <> selfUrl)
        -- I'm not sure if this safe link approach is really useable with this
        -- api hierarchy since I can't access the topmost api from here. Also
        -- authentication would bring a little bit of extra effort as well
        selfUrl = pack . uriPath . linkURI $ safeLink (Proxy @(BaseAPI 1)) (Proxy @(RootCatalog 1))
        start = self
        pagination = Pagination Nothing Nothing
    entries <- map (fromChannel updated) <$> runDB (Channel.userChannels username)
    pure CatalogV1{..}
    where
      fromChannel :: UTCTime -> Channel.Channel -> Entry 1
      fromChannel updated Channel.Channel{..} =
        let url = pack . uriPath . linkURI $ safeLink (Proxy @(BaseAPI 1)) (Proxy @(ChannelCatalog 1)) identifier
            self = Rel ("/api/current/" <> url)
        in EntryV1 channel channel updated channel (Left $ SubSection self)

type VersionedAPI (v :: Nat) = Auth '[SA.BasicAuth, SA.JWT] SafeUser :> BaseAPI v

type RootCatalog (v :: Nat) = "catalog" :> Get '[XML] (Catalog v)
type ChannelCatalog (v :: Nat) = "catalog" :> "channel" :> Capture "channel_id" Channel.ChannelID :> Get '[XML] (Catalog v)
type BaseAPI (v :: Nat) = RootCatalog v
                    :<|> ChannelCatalog v

handler :: forall v. VersionedCatalog AppM v => ServerT (VersionedAPI v) AppM
handler auth = catalogRoot :<|> catalogChannels
  where
    catalogChannels :: Channel.ChannelID -> AppM (Catalog v)
    catalogChannels _ = throwM err403{errBody="Not implemented"}
    catalogRoot :: AppM (Catalog v)
    catalogRoot = flip requireLoggedIn auth getChannels