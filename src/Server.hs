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
module Server where

import qualified API as API
import Server.Auth (authCheck)
import Servant
import Types
import ClassyPrelude hiding (Handler)
import Control.Monad.Logger
import Control.Monad.Except
import Servant.Auth.Server
import Control.Lens
import Data.Generics.Product

type API = API.API :<|> "static" :> Raw


server :: App -> Application
server app = serveWithContext api cfg (enter server' API.handler :<|> serveDirectoryFileServer "static")
  where
    myKey = view (field @"jwk") app
    jwtCfg = defaultJWTSettings myKey
    authCfg = authCheck app
    cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
    server' :: AppM :~> Servant.Handler
    server' = NT (Handler . ExceptT . try . (`runReaderT` app) . (runFileLoggingT "logs/server.log"))
    api :: Proxy API
    api = Proxy
