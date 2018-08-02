module Main where

import Server (server)
import Network.Wai.Handler.Warp (run)

defaultMain :: IO ()
defaultMain = run 8080 server

main :: IO ()
main = defaultMain
