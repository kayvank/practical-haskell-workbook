-- | Chapter7 More Monads
module Chapter7 where

import Chapter7DT
import qualified Control.Monad as M
import Control.Monad.Logic
-- import Control.Monad.Logic
import Control.Monad.Reader
import qualified Control.Monad.Writer as MW hiding (Product)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import qualified Lens.Micro.Platform as L

broken1 :: Integer -> [Integer]
broken1 n = [n -1, n + 1]

broken2 :: Integer -> [Integer]
broken2 n = [1024, n + 2]

b :: [Integer]
b = broken1 73 `mplus` broken2 73

-- exercise 7-2
find_ :: (a -> Bool) -> [a] -> Maybe a
find_ f = M.msum . (map g)
  where
    g = (\x -> if f x then Just x else Nothing)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo =
  foldr
    ( \(Product i t) pinfos ->
        S.insert (InfoPurchasedProduct i) $
          S.insert (InfoPurchasedProductType t) pinfos
    )
    S.empty

purchaseToTransaction :: Purchase -> Transaction
purchaseToTransaction (Purchase c ps) =
  Transaction $
    clientToPurchaseInfo c
      `S.union` productsToPurchaseInfo ps

-- | Exercise 7-3
clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo (GovOrg _) = S.fromList [InfoClientKind KindGovOrg]
clientToPurchaseInfo (Individual p) =
  S.fromList
    [ InfoClientGender (L.view gender p),
      InfoClientKind KindIndividual
    ]
clientToPurchaseInfo (Company c p d) =
  S.fromList
    [ InfoClientGender (L.view gender p),
      InfoClientKind KindCompany,
      InfoClientDuty d
    ]

instance Show AssocRule where
  show (AssocRule l r) = show l ++ " => " ++ show r

setSupport :: [Transaction] -> FrequentSet -> Double
setSupport trans (FrequentSet sElts) =
  let total = length trans
      f (Transaction tElts) = sElts `S.isSubsetOf` tElts
      supp = length (filter f trans)
   in fromIntegral supp / fromIntegral total

ruleConfidence :: [Transaction] -> AssocRule -> Double
ruleConfidence trans (AssocRule l r) =
  setSupport trans (FrequentSet $ l `S.union` r) / setSupport trans (FrequentSet l)

noDups :: Ord a => [a] -> [a]
noDups = S.toList . S.fromList

generateL1 :: Double -> [Transaction] -> [FrequentSet]
generateL1 minSupport transactions =
  noDups $ do
    Transaction t <- transactions
    e <- S.toList t
    let fs = FrequentSet $ S.singleton e
    guard $ setSupport transactions fs > minSupport
    return fs

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x : xs) =
  powerset xs ++ map (x :) (powerset xs)

paths :: [(Int, Int)] -> Int -> Int -> [[Int]]
paths edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        guard $ e_start == start
        subpath <- paths edges e_end end
        return $ start : subpath
   in if start == end
        then return [end] `mplus` e_paths
        else e_paths

addPrefix :: String -> Reader String String
addPrefix s = ask >>= (\p -> return $ p ++ s)

addPrefix' :: [String] -> Reader String [String]
addPrefix' = mapM addPrefix

pathsWriter :: [(Int, Int)] -> Int -> Int -> [[Int]]
pathsWriter edges start end = map MW.execWriter (pathsWriter' edges start end)

pathsWriter' :: [(Int, Int)] -> Int -> Int -> [MW.Writer [Int] ()]
pathsWriter' edges start end =
  let e_paths = do
        (e_start, e_end) <- edges
        M.guard $ e_start == start
        subpath <- pathsWriter' edges e_end end
        return $ do
          MW.tell [start]
          subpath
   in if start == end
        then MW.tell [start] : e_paths
        else e_paths
