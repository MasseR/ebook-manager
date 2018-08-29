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


import Servant
import Servant.HTML.Lucid (HTML)
import Types

import View

import qualified API.Users as Users
import qualified API.Channels as Channels
import qualified API.Books as Books
import qualified API.Catalogue as Catalogue

data Index = Index

type API = Get '[HTML] (AppView Index)
      :<|> Users.API
      :<|> "api" :> "current" :> Channels.API
      :<|> "api" :> "current" :> Books.API
      :<|> "api" :> "1" :> Catalogue.VersionedAPI 1
      :<|> "api" :> "current" :> Catalogue.VersionedAPI 1

handler :: ServerT API AppM
handler = indexHandler
    :<|> Users.handler
    :<|> Channels.handler
    :<|> Books.handler
    :<|> Catalogue.handler
    :<|> Catalogue.handler

instance ToHtml Index where
  toHtml _ = do
    h1_ [class_ "title"] "Home page"
    p_ [class_ "subtitle"] "Hello world"
  toHtmlRaw = toHtml

indexHandler :: AppM (AppView Index)
indexHandler = mkView "Home" Index
