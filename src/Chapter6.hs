{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-#LANGUAGE TemplateHaskell#-}

-- |  Chapter 6, Using Monads

module Chapter6 where

import Data.List
import qualified Data.Map as M
import qualified Lens.Micro.Platform as L
  
class Ord v =>  Vector v where
  distance :: v -> v -> Double
  centroid :: [v] -> v

instance Vector (Double, Double) where
  distance (x1, y1)(x2, y2) = sqrt $ (x2 - x1)**2 + (y2-y1)**2
  centroid vs =
    let
      (x, y) = foldr (\(a, b) (c, d) -> (a+c, b+d)) (0,0) vs
      n = fromIntegral $ length vs
    in (x/n, y/n)


class Vectorizable e v where
  toVector :: e -> v

instance Vectorizable (Double, Double) (Double, Double) where
  toVector = id

-- phase-2 assign points to the cluster of the nearest centroids
-- | v: current centroids, e; which centroid each element corresponds to
clusterAssignmentPhase :: (Ord v, Vector v, Vectorizable e v) => 
  [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase  centroids points =
  let initialMap = M.fromList $ zip centroids (repeat [])
  in  foldr (\p m ->
               let
                 chosenC = minimumBy ( compareDistance p ) centroids
               in M.adjust (p:) chosenC m ) initialMap points
  where compareDistance p x y =
          compare ( distance x $ toVector p ) ( distance y $ toVector p )

-- phase-3 compute new centroids
newCentroidPhase :: (Vector v, Vectorizable e v) =>  M.Map v[e] -> [(v, v)]
newCentroidPhase =  M.toList . fmap (centroid . map toVector)

shouldStop :: (Vector v) => [(v, v)] -> Double -> Bool
shouldStop centroids threshold = foldr( \(x, y) s ->
                                           s + distance x y) 0.0  centroids > threshold

kMeans :: (Vector v, Vectorizable e v)
  => (Int -> [e] -> [v]) -- initialization function
  -> Int  -- number of centroids
  -> [e] -- info
  -> Double -- threshold
  -> [v] -- centroids after convergence 
kMeans i k points  = kMeans' (i k points) points
kMeans' :: (Vector v, Vectorizable e v)
  => [v] -> [e] -> Double -> [v]
kMeans'  centroids points threshold =
  let assignments = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold then
    newCentroids
     else kMeans' newCentroids points threshold

data Client i =
  GovOrg {_identifier :: i, _name :: String}
  | Company {
      _identifier :: i
      , _name :: String,
      _person:: Person
      , _duty :: String }
  | Individual { _identifier :: i, _person :: Person }
  deriving Show
  
data Person = Person {
  _firstName :: String
  , _lastName :: String } deriving Show

fullName :: L.Lens' Person String
fullName = L.lens
  (\(Person f l) -> f ++ " " ++ l)
  (\_ newFullName -> case words newFullName of
      f : l : _ -> Person f l
      _ -> error "Incorrect Name")

L.makeLenses ''Client
L.makeLenses ''Person
