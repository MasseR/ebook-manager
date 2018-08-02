module Main where

import Server (server)
import Network.Wai.Handler.Warp (run)
import Types

defaultMain :: IO ()
defaultMain = run 8080 (server App)

main :: IO ()
main = defaultMain
