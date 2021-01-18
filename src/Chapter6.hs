{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- |  Chapter 6, Using Monads
module Chapter6 where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Lens.Micro.Platform

class Ord v => Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (x1, y1) (x2, y2) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2
  centroid vs =
    let (x, y) = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) vs
        n = fromIntegral $ length vs
     in (x / n, y / n)

class Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

newCentroidPhase :: (Vector v, Vectorizable e v) => M.Map v [e] -> [(v, v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold =
  foldr
    ( \(x, y) s ->
        s + distance x y
    )
    0.0
    centroids
    > threshold

data KMeansState e v = KMeansState
  { _centroids :: [v],
    _points :: [e],
    _err :: Double,
    _threshold :: Double,
    _steps :: Int
  }

makeLenses ''KMeansState

initializeState ::
  (Int -> [e] -> [v]) ->
  Int ->
  [e] ->
  Double ->
  KMeansState e v
initializeState i n pts t =
  KMeansState c pts (1.0 / 0.0) t 0
  where
    c = i n pts

clusterAssignmentPhase ::
  (Ord v, Vector v, Vectorizable e v) =>
  KMeansState e v ->
  M.Map v [e]
-- [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase state =
  let initialMap = M.fromList $ zip centroids_ (repeat [])
   in foldr
        ( \p m ->
            let chosenC = minimumBy (compareDistance p) centroids_
             in M.adjust (p :) chosenC m
        )
        initialMap
        points_
  where
    compareDistance p x y =
      compare (distance x $ toVector p) (distance y $ toVector p)
    centroids_ = view centroids state
    points_ = view points state

kMeans ::
  (Vector v, Vectorizable e v) =>
  (Int -> [e] -> [v]) -> -- initialization function
  Int -> -- number of centroids
  [e] -> -- info
  Double -> -- threshold
  [v] -- centroids after convergence
kMeans i n pts t = view centroids $ kMeans' (initializeState i n pts t)

kMeans' ::
  (Vector v, Vectorizable e v) =>
  KMeansState e v ->
  KMeansState e v
kMeans' state =
  let assignments = clusterAssignmentPhase state
      state1 =
        state & centroids . traversed
          %~ ( \c ->
                 centroid $
                   fmap toVector $
                     M.findWithDefault [] c assignments
             )
      state2 =
        state1 & err
          .~ sum
            ( zipWith
                distance
                (state ^. centroids)
                (state1 ^. centroids)
            )
      state3 = state2 & steps +~ 1
   in if state3 ^. err < state3 ^. threshold then state3 else kMeans' state3

data Client i
  = GovOrg {_identifier :: i, _name :: String}
  | Company
      { _identifier :: i,
        _name :: String,
        _person :: Person,
        _duty :: String
      }
  | Individual {_identifier :: i, _person :: Person}
  deriving (Show)

data Person = Person
  { _firstName :: String,
    _lastName :: String
  }
  deriving (Show)

fullName :: Lens' Person String
fullName =
  lens
    (\(Person f l) -> f ++ " " ++ l)
    ( \_ newFullName -> case words newFullName of
        f : l : _ -> Person f l
        _ -> error "Incorrect Name"
    )

makeLenses ''Client
makeLenses ''Person

purchasesByClientId :: Integer -> [Integer]
purchasesByClientId = undefined

numberItemsByPurchaseId :: Integer -> Maybe Integer
numberItemsByPurchaseId = undefined

productIdByPurchaseId :: Integer -> Maybe Integer
productIdByPurchaseId = undefined

priceByProductId :: Integer -> Maybe Double
priceByProductId = undefined

meanPurchase :: Integer -> Double
meanPurchase clientId =
  let p = purchasesByClientId clientId
   in foldr (+) 0.0 $ catMaybes $ map purchaseValue p

purchaseValue :: Integer -> Maybe Double
purchaseValue purchaseId = do
  n <- numberItemsByPurchaseId purchaseId
  i <- productIdByPurchaseId n
  p <- priceByProductId i
  return $ fromInteger n * p

access :: (s -> a) -> State s a
access f = fmap f get
