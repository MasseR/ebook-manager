{-# Language OverloadedStrings #-}
module Main where

import Server (server)
import Network.Wai.Handler.Warp (run)
import Types
import Configuration (Config)
import Dhall (input, auto)

defaultMain :: Config -> IO ()
defaultMain c = run 8080 (server (App c))

main :: IO ()
main = do
  c <- input auto "./config/config.dhall"
  defaultMain c
