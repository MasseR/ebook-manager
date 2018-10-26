module Main where

import Test.Hspec

spec :: Spec
spec = describe "test" $ it "verifies tests work" $ True == True

main :: IO ()
main = hspec spec
