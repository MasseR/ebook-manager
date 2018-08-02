{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language DuplicateRecordFields #-}
{-# Language TypeApplications #-}
{-# Language DataKinds #-}
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

defaultMain :: Config -> IO ()
defaultMain c = run 8080 (server (App c))

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
