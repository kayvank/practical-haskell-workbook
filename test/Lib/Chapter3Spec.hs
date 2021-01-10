-- |  Chapter3, Code reuse specs

module Lib.Chapter3Spec (
  spec
  ) where

import  Test.Hspec
import Chapter3

spec :: Spec
spec = do
  describe "Chapter 3 specs are: " $ do
    it "filterAsfold specs" $ do
      filterAsFold (even)[5,7,9,12,11,14] `shouldBe` [12,14]
  
    it "mapAsFold specs" $ do
      mapAsFold (+10)[1,2,3,4, 5] `shouldBe` [11,12,13,14, 15]

    it "min sorts a list" $ do
      minSort [9,3,2,10,11,7,8,5] `shouldBe` [2,3,5,7,8,9,10,11]
