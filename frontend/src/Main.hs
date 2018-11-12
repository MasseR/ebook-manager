{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Control.Monad.Trans              (liftIO)
import           Language.Javascript.JSaddle.Warp
import           Miso
import           Miso.String

data Action = Add
            | Subtract
            | SayHello
            | NoOp

newtype Model = Model Int deriving (Eq, Num, ToMisoString)

updateModel :: Action -> Model -> Effect Action Model
updateModel Add m      = noEff (m + 1)
updateModel Subtract m = noEff (m - 1)
updateModel SayHello m = m <# (liftIO (putStrLn "Hello world") >> pure NoOp)
updateModel NoOp m     = noEff m

viewModel :: Model -> View Action
viewModel x =
  div_ [] [ button_  [ onClick Add ] [ text "+" ]
          , text (ms x)
          , button_ [ onClick Subtract ] [ text "-" ]
  ]

main :: IO ()
main = run 8081 $ startApp App{..}
  where
    model = Model 0
    initialAction = SayHello
    update = updateModel
    view = viewModel
    subs = []
    events = defaultEvents
    mountPoint = Nothing
