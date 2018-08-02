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
module Server where

import qualified API as API
import Servant
import Types
import ClassyPrelude hiding (Handler)
import Control.Monad.Logger
import Control.Monad.Except

type API = API.API :<|> "static" :> Raw


server :: App -> Application
server app = serve api (enter server' API.handler :<|> serveDirectoryFileServer "static")
  where
    server' :: AppM :~> Servant.Handler
    server' = NT (Handler . ExceptT . try . (`runReaderT` app) . (runFileLoggingT "logs/server.log"))
    api :: Proxy API
    api = Proxy
