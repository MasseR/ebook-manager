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

import Control.Monad.Trans.Maybe

import qualified Datastore as DS
import Data.ByteArray (convert)
import Crypto.Hash (digestFromByteString)

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
       :<|> "books" :> Capture "book_id" BookID :> "meta" :> ReqBody '[JSON] JsonBook :> Put '[JSON] JsonBook
       :<|> "books" :> Capture "book_id" BookID :> ReqBody '[OctetStream] ByteString :> Put '[JSON] NoContent
       :<|> "books" :> Capture "book_id" BookID :> Get '[OctetStream] ByteString

handler :: ServerT API AppM
handler user = listBooksHandler user
          :<|> postBookMetaHandler user
          :<|> putBookMetaHandler user
          :<|> putBookContentHandler user
          :<|> getBookContentHandler user

getBookContentHandler :: AuthResult SafeUser -> BookID -> AppM ByteString
getBookContentHandler auth bookId = requireBookOwner auth bookId $ \SafeUser{username} -> do
  content <- runMaybeT $ do
    Book{contentHash=mHash} <- MaybeT $ runDB (getBook bookId username)
    contentHash <- MaybeT $ return (mHash >>= digestFromByteString . unHex)
    MaybeT $ DS.get contentHash
  maybe (throwM err404) return content

requireBookOwner :: AuthResult SafeUser -> BookID -> (SafeUser -> AppM a) -> AppM a
requireBookOwner auth bookId f = flip requireLoggedIn auth $ \u@SafeUser{username} -> do
  exists <- runDB $ bookExists bookId
  unless exists $ throwM err404
  runDB (isBookOwner bookId username) >>= \o -> if o then f u else throwM err403

putBookContentHandler :: AuthResult SafeUser -> BookID -> ByteString -> AppM NoContent
putBookContentHandler auth bookId content = requireBookOwner auth bookId $ \SafeUser{username} -> do
  key <- HashDigest . convert <$> DS.put content
  runDB (setContent bookId username key)
  return NoContent

postBookMetaHandler :: AuthResult SafeUser -> PostBook -> AppM JsonBook
postBookMetaHandler auth PostBook{..} = flip requireLoggedIn auth $ \SafeUser{username} -> do
  mIdentifier <- runDB $ insertBook InsertBook{owner=username,..}
  maybe (throwM err403{errBody="Could not insert book"}) (\identifier -> pure JsonBook{..}) mIdentifier


putBookMetaHandler :: AuthResult SafeUser -> BookID -> JsonBook -> AppM JsonBook
putBookMetaHandler auth bookId b@JsonBook{..}
  | bookId == identifier = requireBookOwner auth bookId $ \SafeUser{username=owner} ->
        maybe (throwM err403) (const (return b)) =<< runDB (updateBook UpdateBook{..})
  | otherwise = throwM err403

listBooksHandler :: AuthResult SafeUser -> AppM [JsonBook]
listBooksHandler = requireLoggedIn $ \user -> do
  runDB (usersBooks (view (field @"username") user) >>= mapM augment)
    where
      augment Book{identifier=bookId,contentType,title,description} = do
        channels <- fmap (\Channel{..} -> JsonChannel{..}) <$> booksChannels bookId
        pure JsonBook{identifier=bookId,..}
