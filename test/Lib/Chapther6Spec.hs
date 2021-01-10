-- |  Specs for Chapter6, Knowing your clients using monads

module Lib.Chapther6Spec
( spec
  ) where

import  Chapter6 
import Lens.Micro.Platform 
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter6 specs are: " $ do
    it "calculates person lens correctly" $ do
      let p = Person "John" "Smith"
      (view firstName p, p^.lastName) `shouldBe` ( ("John", "Smith") )
    it "calculates the client lens view correctly" $ do
      let client = Individual 3 (Person "John" "Smith")
      ( view (person . lastName) client ) `shouldBe` "Smith"
  
    it "calculates the client lens seter correctly" $ do
      let client = Individual 3 (Person "John" "Smith")
      let client2 = set identifier 4 client
      ( view (identifier) client2 ) `shouldBe` 4 
  
    it "calculates the client lens view for functions correctly" $ do
      let client = Individual 3 (Person "John" "Smith")
      client^.person.fullName `shouldBe` "John Smith"
  
    it "calculates the client lens setter for functions correctly" $ do
      let client = Individual 3 (Person "John" "Smith")
      let client2 = client & person.fullName .~ "Marianne Kox"
      client2^.person.lastName `shouldBe` "Kox"
      client2^.person.firstName `shouldBe` "Marianne"


