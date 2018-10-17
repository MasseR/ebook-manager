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

import qualified API.Books
import           ClassyPrelude
import           Database
import           Database.Book (Book(..))
import qualified Database.Channel as Channel
import           GHC.TypeLits
import           Servant hiding (contentType)
import           Servant.Auth as SA
import           Servant.XML
import           Server.Auth
import           Types

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
  getBooks :: Channel.ChannelID -> SafeUser -> m (Catalog v)

instance VersionedCatalog AppM 1 where
  getChannels = getChannelsV1
  getBooks = getBooksV1

relUrl :: Link -> Rel
relUrl x = Rel ("/api/current/" <> (pack . uriPath . linkURI $ x))

getBooksV1 :: Channel.ChannelID -> SafeUser -> AppM (Catalog 1)
getBooksV1 channelID SafeUser{username} = do
  updated <- liftIO getCurrentTime
  let self = relUrl selfUrl
      start = relUrl startUrl
      selfUrl = safeLink (Proxy @(BaseAPI 1)) (Proxy @(ChannelCatalog 1)) channelID
      startUrl = safeLink (Proxy @(BaseAPI 1)) (Proxy @(RootCatalog 1))
      pagination = Pagination Nothing Nothing
  entries <- map (toEntry updated) <$> runDB (Channel.channelBooks username channelID)
  pure CatalogV1{..}
  where
    toEntry updated Book{description,title,identifier=bookId} =
      let content = fromMaybe "no content" description
          identifier = pack . show $ bookId
          link = Right (Acquisition (relUrl (safeLink (Proxy @API.Books.BaseAPI) (Proxy @API.Books.GetBook) bookId)))
      in EntryV1{..}

getChannelsV1 :: SafeUser -> AppM (Catalog 1)
getChannelsV1 SafeUser{username} = do
  updated <- liftIO getCurrentTime
  let self = relUrl selfUrl
      -- I'm not sure if this safe link approach is really useable with this
      -- api hierarchy since I can't access the topmost api from here. Also
      -- authentication would bring a little bit of extra effort as well
      selfUrl = safeLink (Proxy @(BaseAPI 1)) (Proxy @(RootCatalog 1))
      start = self
      pagination = Pagination Nothing Nothing
  entries <- map (fromChannel updated) <$> runDB (Channel.userChannels username)
  pure CatalogV1{..}
  where
    fromChannel :: UTCTime -> Channel.Channel -> Entry 1
    fromChannel updated Channel.Channel{..} =
      let url = safeLink (Proxy @(BaseAPI 1)) (Proxy @(ChannelCatalog 1)) identifier
          self = relUrl url
      in EntryV1 channel channel updated channel (Left $ SubSection self)

type VersionedAPI (v :: Nat) = Auth '[SA.BasicAuth, SA.JWT] SafeUser :> BaseAPI v

type CatalogContent = '[XML, OPDS]

type RootCatalog (v :: Nat) = "catalog" :> Get CatalogContent (Catalog v)
type ChannelCatalog (v :: Nat) = "catalog" :> "channel" :> Capture "channel_id" Channel.ChannelID :> Get CatalogContent (Catalog v)
type BaseAPI (v :: Nat) = RootCatalog v
                    :<|> ChannelCatalog v

handler :: forall v. VersionedCatalog AppM v => ServerT (VersionedAPI v) AppM
handler auth = catalogRoot :<|> catalogChannels
  where
    catalogChannels :: Channel.ChannelID -> AppM (Catalog v)
    -- Channel specific catalog returns tags inside the catalog
    catalogChannels identifier = flip requireLoggedIn auth (getBooks identifier)
    catalogRoot :: AppM (Catalog v)
    -- catalog root returns channels
    catalogRoot = flip requireLoggedIn auth getChannels
