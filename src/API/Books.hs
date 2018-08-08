{-# Language DuplicateRecordFields #-}
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
{-# Language NamedFieldPuns #-}
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

data JsonBook = JsonBook { identifier :: BookID
                         , contentType :: Text
                         , title :: Maybe Text
                         , description :: Maybe Text
                         , channels :: [JsonChannel] }
              deriving (Generic, Show)

data PostBook = PostBook { contentType :: Text
                         , title :: Maybe Text
                         , description :: Maybe Text
                         , channels :: [JsonChannel] }
              deriving (Generic, Show)


instance ToJSON JsonBook
instance FromJSON JsonBook
instance ToJSON PostBook
instance FromJSON PostBook

type API = Auth '[SA.BasicAuth, SA.JWT] SafeUser :> BaseAPI

type BaseAPI = "books" :> Get '[JSON] [JsonBook]
       :<|> "books" :> ReqBody '[JSON] PostBook :> Post '[JSON] JsonBook
       :<|> "books" :> Capture "book_id" BookID :> ReqBody '[JSON] JsonBook :> Post '[JSON] JsonBook
      --  :<|> "books" :> Capture "book_id" BookID :> ReqBody '[JSON] JsonBook :> PUT JsonBook

handler :: ServerT API AppM
handler user = listBooksHandler user :<|> postBookMetaHandler user :<|> putBookMetaHandler user

postBookMetaHandler :: AuthResult SafeUser -> PostBook -> AppM JsonBook
postBookMetaHandler auth PostBook{..} = flip requireLoggedIn auth $ \SafeUser{username} -> do
  mIdentifier <- runDB $ insertBook InsertBook{owner=username,..}
  maybe (throwM err403{errBody="Could not insert book"}) (\identifier -> pure JsonBook{..}) mIdentifier


putBookMetaHandler :: AuthResult SafeUser -> BookID -> JsonBook -> AppM JsonBook
putBookMetaHandler auth bookId b@JsonBook{..}
  | bookId == identifier = flip requireLoggedIn auth $ \SafeUser{username=owner} ->
        maybe (throwM err403) (const (return b)) =<< runDB (updateBook UpdateBook{..})
  | otherwise = throwM err403

listBooksHandler :: AuthResult SafeUser -> AppM [JsonBook]
listBooksHandler = requireLoggedIn $ \user -> do
  runDB (usersBooks (view (field @"username") user) >>= mapM augment)
    where
      augment Book{identifier=bookId,contentType,title,description} = do
        channels <- fmap (\Channel{..} -> JsonChannel{..}) <$> booksChannels bookId
        pure JsonBook{identifier=bookId,..}
