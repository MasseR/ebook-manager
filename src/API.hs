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
module API (API, handler) where


import ClassyPrelude hiding (Handler, Index)
import Servant
import Servant.HTML.Lucid (HTML)
import Lucid (HtmlT, ToHtml(..))
import qualified Lucid.Html5 as H
import Types
import Control.Monad.Logger

import qualified API.Users as Users

-- XXX: Temporary
import Database.Schema
import Database

data Index = Index

bulma :: Monad m => HtmlT m ()
bulma = H.doctypehtml_ $ do
  H.meta_ [ H.name_ "viewport", H.content_ "width=device-width, initial-scale=1" ]
  H.meta_ [ H.charset_ "utf-8" ]
  H.title_ "Hello bulma!"
  H.link_ [ H.rel_ "stylesheet", H.href_ "/static/css/bulma.min.css" ]
  H.script_ [ H.defer_ "", H.src_ "https://use.fontawesome.com/releases/v5.1.0/js/all.js" ] ("" :: String)
  H.body_ $ do
    H.section_ [ H.class_ "section" ] $ do
      H.div_ [ H.class_ "container" ] $ do
        H.h1_ [ H.class_ "title" ] "Hello world"
        H.p_ [ H.class_ "subtitle" ] "My first website with bulma"

instance ToHtml Index where
  toHtml _ = bulma
  toHtmlRaw = toHtml

type API = Get '[HTML] Index
      :<|> Users.API

handler :: ServerT API AppM
handler = indexHandler :<|> Users.handler

indexHandler :: AppM Index
indexHandler = do
  u <- runDB $ do
    query $ select $ gen users
  $logInfo $ "users: " <> (pack . show $ u)
  return Index
