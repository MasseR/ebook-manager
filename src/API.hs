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

data Index = Index

type API = Get '[HTML] (AppView Index)
      :<|> Users.API
      :<|> Channels.API
      :<|> Books.API

handler :: ServerT API AppM
handler = indexHandler
    :<|> Users.handler
    :<|> Channels.handler
    :<|> Books.handler

instance ToHtml Index where
  toHtml _ = do
    h1_ [class_ "title"] "Home page"
    p_ [class_ "subtitle"] "Hello world"
  toHtmlRaw = toHtml

indexHandler :: AppM (AppView Index)
indexHandler = mkView "Home" Index
