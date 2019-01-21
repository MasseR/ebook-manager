{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
module Main where

import           ClassyPrelude
import           Configuration
import           Control.Lens              (view)
import           Data.Generics.Product
import           Data.Pool                 (createPool)
import           Database.Selda.PostgreSQL (PGConnectInfo (..), pgOpen,
                                            seldaClose)
import           Dhall                     (auto, input)
import           Network.Wai.Handler.Warp  (run)
import           Servant.Auth.Server       (generateKey)
import           Server                    (server)
import           Types
import System.Environment (getEnvironment)

defaultMain :: App -> IO ()
defaultMain = run 8080 . server

withApp :: Config -> (App -> IO ()) -> IO ()
withApp config f = do
  let pgHost = view (field @"database" . field @"host") config
      pgPort = 5432
      pgSchema = Nothing
      pgDatabase = view (field @"database" . field @"database") config
      pgUsername = Just (view (field @"database" . field @"username") config)
      pgPassword = Just (view (field @"database" . field @"password") config)
  database <- createPool (pgOpen (PGConnectInfo{..})) seldaClose 10 2 5
  jwk <- generateKey
  f App{..}

main :: IO ()
main = do
  path <- fmap pack . lookup "CONF" <$> getEnvironment
  c <- input auto (fromMaybe "./config/config.dhall" path)
  withApp c defaultMain
