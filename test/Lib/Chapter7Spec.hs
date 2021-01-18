-- |  Chapter 7, More Monad
module Lib.Chapter7Spec
  ( spec,
  )
where

import qualified Chapter7 as CH7
import qualified Control.Monad as M
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter 7 specs are:" $ do
    it "mplus specs are" $ do
      Just 1 `M.mplus` Just 2 `shouldBe` Just 1
      Nothing `M.mplus` Just 2 `shouldBe` Just 2
      Just 1 `M.mplus` Nothing `shouldBe` Just 1
      [1, 2, 3] `M.mplus` [4, 5, 6] `shouldBe` [1, 2, 3, 4, 5, 6]
      CH7.b `shouldBe` [72, 74, 1024, 75]

    it "specs for Exercise 7-2, find_" $ do
      CH7.find_ odd [2 .. 10] `shouldBe` Just 3
      CH7.find_ (> 8) [2 .. 10] `shouldBe` Just 9
