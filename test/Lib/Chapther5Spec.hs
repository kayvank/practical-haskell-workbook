-- |  Specs for Laziness and Infinite Structures, Chapter 5
module Lib.Chapther5Spec
  ( spec,
  )
where

import           Chapter5
import           Test.Hspec

spec :: Spec
spec = do
  describe "Chapter5 specs are:" $ do
    it "evaluates timestream machine lazily " $ do
      let tm2019 = newerTimeMachines 2018 timelyIncMachines
      tm2019 `shouldBe` Just (TM "Timely Inc." 2019)
    it "zips list lazily" $ do
      let indexedList = allNumbers `zip` "abc"
      indexedList `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]
    it "calculates fibonacci number lazily" $ do
      fibonacci !! 20 `shouldBe` 6765
    it "computes primes " $ do
      primesTo 10 `shouldBe` [2, 3, 5, 7]
