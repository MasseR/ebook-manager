{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Server where

import qualified API
import           ClassyPrelude         hiding (Handler)
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Generics.Product
import           Servant
import           Servant.Auth.Docs     ()
import           Servant.Auth.Server   as SAS
import qualified Servant.Docs          as Docs
import           Servant.HTML.Lucid    (HTML)
import           Server.Auth           (SafeUser, authCheck)
import           Types

type API = API.API
  :<|> "api" :> "help" :> Get '[PlainText, HTML] String

type Ctx = '[BasicAuthData -> IO (AuthResult SafeUser), CookieSettings, JWTSettings]

server :: App -> Application
server app = serveWithContext api cfg (hoistServerWithContext (Proxy @ API.API) (Proxy @ Ctx) server' API.handler :<|> serveDocs)
  where
    apiDocs :: Docs.API
    apiDocs = Docs.docs (Proxy @API.API)
    serveDocs = pure $ Docs.markdown apiDocs
    myKey = view (field @"jwk") app
    jwtCfg = defaultJWTSettings myKey
    authCfg = authCheck app
    cookieSettings = SAS.defaultCookieSettings{cookieIsSecure=SAS.NotSecure}
    cfg = jwtCfg :. cookieSettings :. authCfg :. EmptyContext
    server' :: AppM a -> Servant.Handler a
    server' = Handler . ExceptT . try . (`runReaderT` app) . runFileLoggingT "logs/server.log"
    api :: Proxy API
    api = Proxy
