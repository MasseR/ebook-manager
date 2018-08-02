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

type API = API.API :<|> "static" :> Raw

handler :: ServerT API Handler
handler = API.handler :<|> serveDirectoryFileServer "static"


server :: Application
server = serve api handler
  where
    api :: Proxy API
    api = Proxy
