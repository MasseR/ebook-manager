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
{-# Language ScopedTypeVariables #-}
module Server where

import qualified API as API
import Server.Auth (authCheck)
import Servant
import Types
import ClassyPrelude hiding (Handler)
import Control.Monad.Logger
import Control.Monad.Except
import Servant.Auth.Server as SAS
import Control.Lens
import Data.Generics.Product
import Server.Auth (SafeUser)

type API = API.API :<|> "static" :> Raw

type Ctx = '[BasicAuthData -> IO (AuthResult SafeUser), CookieSettings, JWTSettings]

server :: App -> Application
server app = serveWithContext api cfg (hoistServerWithContext (Proxy @ API.API) (Proxy @ Ctx) server' API.handler :<|> serveDirectoryFileServer "static")
  where
    myKey = view (field @"jwk") app
    jwtCfg = defaultJWTSettings myKey
    authCfg = authCheck app
    cookieSettings = SAS.defaultCookieSettings{cookieIsSecure=SAS.NotSecure}
    cfg = jwtCfg :. cookieSettings :. authCfg :. EmptyContext
    server' :: AppM a -> Servant.Handler a
    server' = Handler . ExceptT . try . (`runReaderT` app) . (runFileLoggingT "logs/server.log")
    api :: Proxy API
    api = Proxy
