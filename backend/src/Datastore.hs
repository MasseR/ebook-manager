{-# Language TypeFamilies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}
module Datastore where

import ClassyPrelude
import Types
import Crypto.Hash
import Data.Generics.Product
import Control.Lens
import System.Directory (doesFileExist, createDirectoryIfMissing)

-- I might change the implementation at some point
class Monad m => MonadDS m where
  type Key m :: *

  put :: ByteString -> m (Key m)
  get :: Key m -> m (Maybe ByteString)

instance MonadDS AppM where
  type Key AppM = Digest SHA256

  put = putLocal
  get = getLocal

putLocal :: ( MonadIO m
            , HasField "config" r r config config
            , HasField "store" config config store store
            , HasType Text store
            , MonadReader r m)
            => ByteString -> m (Digest SHA256)
putLocal bs = do
  store :: FilePath <- unpack <$> view (field @"config" . field @"store" . typed @Text)
  liftIO $ createDirectoryIfMissing True store
  let key = hashWith SHA256 bs
  writeFile (store </> show key) bs
  return key

getLocal :: ( MonadIO m
            , HasField "config" r r config config
            , HasField "store" config config store store
            , HasType Text store
            , MonadReader r m)
            => Digest SHA256 -> m (Maybe ByteString)
getLocal key = do
  store <- unpack <$> view (field @"config" . field @"store" . typed @Text)
  liftIO $ createDirectoryIfMissing True store
  let file = store </> show key
  exists <- liftIO $ doesFileExist file
  if exists then Just <$> readFile file else pure Nothing
