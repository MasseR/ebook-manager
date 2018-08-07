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
{-# Language DataKinds #-}
module API.Books where

import Servant hiding (contentType)
import Types
import ClassyPrelude
import Server.Auth
import Servant.Auth as SA
import Data.Aeson
import API.Channels (JsonChannel(..))
import Database.Book
import Database.Channel
import Database
import Control.Lens
import Data.Generics.Product

data JsonBook = JsonBook { contentType :: Text
                         , title :: Maybe Text
                         , description :: Maybe Text
                         , channels :: [JsonChannel] }
              deriving (Generic, Show)


instance ToJSON JsonBook
instance FromJSON JsonBook

type API = Auth '[SA.BasicAuth, SA.JWT] SafeUser :> BaseAPI

type BaseAPI = "books" :> Get '[JSON] [JsonBook]
      --  :<|> "books" :> ReqBody '[JSON] JsonBook :> PUT JsonBook
      --  :<|> "books" :> Param "book_id" BookID :> ReqBody '[JSON] JsonBook :> PUT JsonBook

handler :: ServerT API AppM
handler user = listBooksHandler user

listBooksHandler :: AuthResult SafeUser -> AppM [JsonBook]
listBooksHandler = requireLoggedIn $ \user -> do
  runDB (usersBooks (view (field @"username") user) >>= mapM augment)
    where
      augment Book{identifier=bookId,..} = do
        channels <- fmap (\Channel{..} -> JsonChannel{..}) <$> booksChannels bookId
        pure JsonBook{..}
