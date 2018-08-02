{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
{-# Language NoImplicitPrelude #-}
module Main where

import Server (server)
import Network.Wai.Handler.Warp (run)
import Types
import Configuration
import Dhall (input, auto)
import System.Process (callProcess)
import ClassyPrelude
import Control.Lens (view)
import Data.Generics.Product
import Data.Pool (createPool)
import Database.Selda.PostgreSQL (PGConnectInfo(..), pgOpen, seldaClose)

defaultMain :: Config -> IO ()
defaultMain config = do
  let pgHost = view (field @"database" . field @"host") config
      pgPort = 5432
      pgDatabase = view (field @"database" . field @"database") config
      pgUsername = Just (view (field @"database" . field @"username") config)
      pgPassword = Just (view (field @"database" . field @"password") config)
  database <- createPool (pgOpen (PGConnectInfo{..})) seldaClose 10 2 5
  let app = App{..}
  run 8080 (server app)

migrate :: Pg -> IO ()
migrate Pg{..} = do
  -- Credentials visible on ps
  -- XXX: Modify this to write the credentials to a temporary file or something
  callProcess "flyway" $ fmap unpack [ "migrate"
                                     , "-locations=filesystem:migrations/"
                                     , "-url=jdbc:postgresql://" <> host <> "/" <> database
                                     , "-user=" <> username
                                     , "-password=" <> password]

main :: IO ()
main = do
  c <- input auto "./config/config.dhall"
  migrate (view (field @"database") c)
  defaultMain c
