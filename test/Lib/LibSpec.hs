module Lib.LibSpec (spec) where

import           Lib        (absalute)
import           Test.Hspec

spec :: Spec
spec = do
  describe "Chapter 1 specs are:" $ do
    it "passes the trivial test" $ do
      1 `shouldBe` 1

-- it "returns positive number when given a negative input" $ do
--   absalute (-1) `shouldBe` 1
