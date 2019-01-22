{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module API (API, handler) where


import           Servant
import           Types

import qualified API.Books          as Books
import qualified API.Catalogue      as Catalogue
import qualified API.Channels       as Channels
import qualified API.Users          as Users

type API = "api" :> Users.API
      :<|> "api" :> "current" :> Channels.API
      :<|> "api" :> "current" :> Books.API
      :<|> "api" :> "1" :> Catalogue.VersionedAPI 1
      :<|> "api" :> "current" :> Catalogue.VersionedAPI 1

handler :: ServerT API AppM
handler = Users.handler
    :<|> Channels.handler
    :<|> Books.handler
    :<|> Catalogue.handler
    :<|> Catalogue.handler

