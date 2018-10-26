{-# Language TypeApplications #-}
module Main where

import           API.Books
import qualified Data.Aeson as A
import           Data.Char (isPrint)
import           Data.GenValidity.Text ()
import qualified Data.Text as T
import           Database.Schema
import           Prelude
import           Test.Hspec
import           Test.Validity

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
    it "Works for PlainPassword" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @PlainPassword) A.decode
    it "Works for Email" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @Email) A.decode
    it "Username" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @Username) A.decode
    it "Works for BookID" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @BookID) A.decode
    it "Works for ChannelID" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @ChannelID) A.decode
    it "Works for Role" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @Role) A.decode
    it "Works for Visibility" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @Visibility) A.decode
    it "Works for JsonBook" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @JsonBook) A.decode
    it "Works for PostBook" $ inverseFunctionsIfSecondSucceedsOnValid (A.encode @PostBook) A.decode

main :: IO ()
main = hspec spec
