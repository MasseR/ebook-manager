{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs              #-}
module Main where

import           Control.Lens                     (over, set)
import           Control.Monad.Trans              (liftIO)
import           Data.Generics.Product
import           GHC.Generics                     (Generic)
import           Language.Javascript.JSaddle.Warp
import           Miso                             hiding (set)
import           Miso.String
import           Servant.API
import           Servant.Links
import Data.Proxy (Proxy(..))

type API = Home :<|> Login :<|> Register
type Home = View Action
type Login = "login" :> View Action
type Register = "register" :> View Action

data Action = Add
            | Subtract
            | SayHello
            | HandleURI URI
            | ChangeURI URI
            | NoOp

data Model = Model { counter :: Int
                   , uri     :: URI }
  deriving (Eq, Generic)

updateModel :: Model -> Action -> Effect Action Model
updateModel m = \case
  Add -> noEff (over (field @"counter") (+1) m)
  Subtract -> noEff (over (field @"counter") (\x -> x - 1) m)
  SayHello -> m <# (liftIO (putStrLn "Hello world") >> pure NoOp)
  HandleURI uri -> noEff (set (field @"uri") uri m)
  ChangeURI uri -> m <# do
    liftIO $ putStrLn $ "Pushing uri " <> show uri
    pushURI uri
    return $ HandleURI uri
  NoOp -> noEff m

viewModel :: Model -> View Action
viewModel model = view
  where
    view = either (const the404) id $ runRoute @API Proxy handlers uri model
    handlers = home :<|> login :<|> register
    home _ = div_ [] [ button_  [ onClick Add ] [ text "+" ]
              , text (ms (counter model))
              , button_ [ onClick Subtract ] [ text "-" ]
              , button_ [ onClick goLogin ] [ text "go login" ]
              , button_ [ onClick goRegister ] [ text "go register" ]
            ]
    login _ = div_ [] []
    register _ = div_ [] [
        h3_ [] [text "register"]
      , label_ [] [text "Username"], input_ [id_ "username", name_ "username"]
      , label_ [] [text "Email"], input_ [id_ "email", name_ "email"]
      , label_ [] [text "Password"], input_ [id_ "password", name_ "password"]
      , label_ [] [text "Password again"], input_ [id_ "passwordAgain", name_ "passwordAgain"]
      , button_ [] [text "Register"]
      ]
    the404 = div_ [] []

goLogin, goHome, goRegister :: Action
goLogin = goto @Login @API Proxy Proxy
goHome = goto @Home @API Proxy Proxy
goRegister = goto @Register @API Proxy Proxy

goto :: (IsElem endpoint api, HasLink endpoint, MkLink endpoint Link ~ Link) => Proxy api -> Proxy endpoint -> Action
goto a b = ChangeURI (linkURI (safeLink a b))

main :: IO ()
main = run 8081 $ do
  model <- mkModel
  startApp App{..}
  where
    mkModel = Model <$> pure 0 <*> getCurrentURI
    initialAction = SayHello
    update = flip updateModel
    view = viewModel
    subs = [ uriSub HandleURI ]
    events = defaultEvents
    mountPoint = Nothing
