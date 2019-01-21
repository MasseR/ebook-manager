{-# LANGUAGE TypeApplications #-}
module Main where

import           API.Books
import qualified Data.Aeson            as A
import           Data.Char             (isPrint)
import           Data.GenValidity.Text ()
import qualified Data.Text             as T
import           Database.Schema
import           Prelude
import           Test.Hspec
import           Test.Validity
import           Test.Validity.Aeson

instance GenUnchecked PlainPassword
instance GenValid PlainPassword
instance GenInvalid PlainPassword
instance Validity PlainPassword
instance GenUnchecked Email
instance GenValid Email
instance GenInvalid Email
instance Validity Email
instance GenUnchecked Username
instance GenValid Username
instance GenInvalid Username
instance Validity Username
instance GenUnchecked BookID
instance GenValid BookID
instance GenInvalid BookID
instance Validity BookID
instance GenUnchecked ChannelID
instance GenValid ChannelID
instance GenInvalid ChannelID
instance Validity ChannelID
instance GenUnchecked Role
instance GenValid Role
instance GenInvalid Role
instance Validity Role
instance GenUnchecked Visibility
instance GenValid Visibility
instance GenInvalid Visibility
instance Validity Visibility
instance GenUnchecked JsonBook
instance GenValid JsonBook
instance GenInvalid JsonBook
instance Validity JsonBook
instance GenUnchecked PostBook
instance GenValid PostBook
instance GenInvalid PostBook
instance Validity PostBook

spec :: Spec
spec = do
  describe "JSON encoding" $ do
    jsonSpecOnValid @PlainPassword
    jsonSpecOnValid @Email
    jsonSpecOnValid @Username
    jsonSpecOnValid @BookID
    jsonSpecOnValid @ChannelID
    jsonSpecOnValid @Role
    jsonSpecOnValid @Visibility
    jsonSpecOnValid @JsonBook
    jsonSpecOnValid @PostBook

main :: IO ()
main = hspec spec
