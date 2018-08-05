{-# Language NoImplicitPrelude #-}
module View
  ( AppView
  , mkView
  , ToHtml(..)
  , module H )
  where

import ClassyPrelude
import Server.Auth
import Lucid (HtmlT, ToHtml(..))
import Lucid.Html5 as H

-- Idea from stackbuilders
-- The idea hasn't been fleshed out 100% yet, but basically for every html view
-- have the endpoint return an @AppView@. Might work with status codes as well
--
-- Collect the metadata to the data type and try to manage it automatically
data AppView view = AppView { content :: view
                            , title :: Text
                            , user :: AuthResult SafeUser
                            } deriving (Generic, Show)

instance (ToHtml view) => ToHtml (AppView view) where
  toHtml v = bulma v
  toHtmlRaw = toHtml

-- Not sure if the monad constraint is needed. Maybe in the future?
mkView :: (Monad m, ToHtml view) => Text -> view -> m (AppView view)
mkView title content = mkAuthView title content Indefinite

mkAuthView :: (Monad m, ToHtml view) => Text -> view -> AuthResult SafeUser -> m (AppView view)
mkAuthView title content user = pure AppView{..}

bulma :: (Monad m, ToHtml view) => AppView view -> HtmlT m ()
bulma AppView{..} = H.doctypehtml_ $ do
  H.meta_ [ H.name_ "viewport", H.content_ "width=device-width, initial-scale=1" ]
  H.meta_ [ H.charset_ "utf-8" ]
  H.title_ "Hello bulma!"
  H.link_ [ H.rel_ "stylesheet", H.href_ "/static/css/bulma.min.css" ]
  H.title_ (toHtml title)
  H.script_ [ H.defer_ "", H.src_ "https://use.fontawesome.com/releases/v5.1.0/js/all.js" ] ("" :: String)
  H.body_ $ do
    H.section_ [ H.class_ "section" ] $ do
      H.div_ [ H.class_ "container" ] $ toHtml content
