-- | Chapter 5, Laziness and Infinite Structures
module Chapter5 where

import           Data.List

data TimeMachine = TM {manufacturer :: String, year :: Integer}
  deriving (Eq, Show)

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y + 1)

timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100

newerTimeMachines :: Integer -> [TimeMachine] -> Maybe TimeMachine
newerTimeMachines _year = find (\(TM {year = y}) -> y > _year)

allNumbersFrom :: Integer -> [Integer]
allNumbersFrom n = n : allNumbersFrom (n + 1)

allNumbers :: [Integer]
allNumbers = allNumbersFrom 0

fibonacci, fibonacci2 :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
fibonacci2 = undefined

primesTo :: Integer -> [Integer]
primesTo k = sieve [2 .. k]
  where
    sieve :: [Integer] -> [Integer]
    sieve []       = []
    sieve (x : xs) = x : sieve (xs \\ [x, x + x .. k])

primes :: [Integer]
primes = concat $ primesTo <$> [2 ..]

