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
import ClassyPrelude
import Control.Lens (view)
import Data.Generics.Product
import Data.Pool (createPool)
import Database.Selda.PostgreSQL (PGConnectInfo(..), pgOpen, seldaClose)

defaultMain :: App -> IO ()
defaultMain = run 8080 . server

withApp :: Config -> (App -> IO ()) -> IO ()
withApp config f = do
  let pgHost = view (field @"database" . field @"host") config
      pgPort = 5432
      pgDatabase = view (field @"database" . field @"database") config
      pgUsername = Just (view (field @"database" . field @"username") config)
      pgPassword = Just (view (field @"database" . field @"password") config)
  database <- createPool (pgOpen (PGConnectInfo{..})) seldaClose 10 2 5
  f App{..}

main :: IO ()
main = do
  c <- input auto "./config/config.dhall"
  withApp c defaultMain
