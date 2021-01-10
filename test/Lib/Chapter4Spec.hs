{-# LANGUAGE ScopedTypeVariables #-}
-- | 

module Lib.Chapter4Spec (
  spec
  ) where

import  Test.Hspec
import Data.Tree 
import Chapter4

spec :: Spec
spec = do
  describe "Chpater 4 specs are: " $ do
    it "classify the clinets correctly" $ do
      True `shouldBe` True
    it "preOrder traversal" $ do
      let pictureTree :: Tree Int =
            Node 1 [ Node 2 [
                       Node 3 []
                       , Node 4 []
                       , Node 5 [] ]
                   , Node 6 [] ]
      ["1","2","3","4","5","6"] `shouldBe`
        preOrder show pictureTree
