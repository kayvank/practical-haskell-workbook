{-# LANGUAGE InstanceSigs #-}
-- | chapter 4, Sample code and Exercise

module Chapter4 where

import           Chapter3
import qualified Data.Graph  as G
import qualified Data.Map    as M hiding (foldr, map)
import           Data.Monoid
import           Data.Set    as S hiding (foldr, map)
import qualified Data.Tree   as T
-- import           Prelude     hiding (fmap)

data ClientKind =
  GovOrgKind
  | CompanyKind
  | IndividualKind
  deriving (Show, Eq, Ord)

type ClientInt = Client Integer
type ClientIntSet = S.Set ClientInt

clasifyClients :: [ClientInt] -> M.Map ClientKind ClientIntSet
clasifyClients = foldr f M.empty where
  f
    :: ClientInt
    -> M.Map ClientKind ClientIntSet
    -> M.Map ClientKind ClientIntSet
  f (GovOrg i s) m =
    M.insertWith S.union (GovOrgKind) (S.singleton (GovOrg i s)) m
  f (Company i s p d) m =
    M.insertWith S.union (CompanyKind) (S.singleton (Company i s p d)) m
  f (Individual i s) m =
    M.insertWith S.union (IndividualKind) (S.singleton (Individual i s)) m
-- ^ Tree section
-- data Tree a - Node { rootLabel :: a, subForest :: Forest a }
-- type Forest a = [Tree a]

preOrder :: (a -> b) -> T.Tree a -> [b]
preOrder f (T.Node v subtrees) =
  let subtreesTraversed = g subtrees in f v : subtreesTraversed
  where g = concat . (map $ preOrder f)

postOrder :: (a -> b) -> T.Tree a -> [b]
postOrder f (T.Node v subtrees) =
  let subtreesTraversed = g subtrees in subtreesTraversed ++ [(f v)]
  where g = concat . (map $ preOrder f)

-- ^ Grapsh section
-- graphFromEdges :: Ord key => [(node, key, [key])] ->
           -- (Graph, Vertex -> (node, key, [key])
           -- , key -> Maybe Vertex)
timeMachineGraph
  :: [  -- keys
      (String  -- valued
             , String -- key
                     , [String])]
timeMachineGraph =
  [ ("wood"    , "wood"    , ["walls"])
  , ("plastic" , "plastic" , ["walls", "wheels"])
  , ("aluminum", "aluminum", ["wheels", "door"])
  , ("walls"   , "walls"   , ["done"])
  , ("wheels"  , "wheels"  , ["done"])
  , ("door"    , "door"    , ["done"])
  , ("done"    , "done"    , [])
  ]

timeMachinePrecedence
  :: (G.Graph, G.Vertex -> (String, String, [String]), String -> Maybe G.Vertex)
timeMachinePrecedence = G.graphFromEdges timeMachineGraph

workflow' :: [Int]
workflow' = G.topSort g where (g, _, _) = timeMachinePrecedence

workflow :: [String]
workflow =
  let (g, v, _) = timeMachinePrecedence
  in  map (\x -> let (k, _, _) = v x in k) $ G.topSort g

timeMachineTravel :: G.Graph
timeMachineTravel = G.buildG
  (103, 2013)
  [ (1302, 1614)
  , (1614, 1302)
  , (1302, 2013)
  , (2013, 1302)
  , (1614, 2013)
  , (2013, 1408)
  , (1408, 1993)
  , (1408, 917)
  , (1993, 917)
  , (917 , 103)
  , (103 , 917)
  ]

class Nameable n where
  name :: n -> String

initial :: Nameable n => n -> Char
initial n = head $ name n

instance Nameable (Client i) where
  name Individual { clientId = _, person = Person { firstName = f, lastName = l } }
    = f ++ " " ++ l
  name c = clientName c

class Priceable p where
  getPrice :: p -> Double

totalPrice :: Priceable p => [p] -> Double
totalPrice ps = foldr (\x y -> (getPrice x) + y) 0.0 ps

data TravelGuid = TravelGuid
  {
  title     :: String
  , authors :: String
  , price   :: Double
  } deriving (Show, Eq, Ord)

data BinaryTree1 = Node1 TravelGuid  BinaryTree1 BinaryTree1 | Leaf1 deriving Show

treeFind1 :: TravelGuid -> BinaryTree1 -> Maybe TravelGuid
treeFind1 _ Leaf1         = Nothing
treeFind1 t (Node1 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind1 t l
  GT -> treeFind1 t r
treeInsert1 :: TravelGuid -> BinaryTree1 -> BinaryTree1
treeInsert1 t Leaf1           = Node1 t Leaf1 Leaf1
treeInsert1 t n@(Node1 v l r) = case compare t v of
  EQ -> n
  LT -> Node1 v (treeInsert1 t l) r
  GT -> Node1 v l (treeInsert1 t r)

data BinaryTree2 a = Node2 a (BinaryTree2 a) (BinaryTree2 a) | Leaf2 deriving Show

treeFind2 :: Ord a => a -> BinaryTree2 a -> Maybe a
treeFind2 _ Leaf2         = Nothing
treeFind2 t (Node2 v l r) = case compare t v of
  EQ -> Just v
  LT -> treeFind2 t l
  GT -> treeFind2 t r

treeInsert2 :: Ord a => a -> BinaryTree2 a -> BinaryTree2 a
treeInsert2 t Leaf2           = Node2 t Leaf2 Leaf2
treeInsert2 t n@(Node2 v l r) = case compare t v of
  EQ -> n
  LT -> Node2 v (treeInsert2 t l) r
  GT -> Node2 v l (treeInsert2 t r)


newtype TGByPrice = TGByPrice TravelGuid deriving Eq

instance Ord TGByPrice where
  (TGByPrice (TravelGuid t1 a1 p1)) <= (TGByPrice (TravelGuid t2 a2 p2)) =
    p1 < p2 || (p1 == p2 && (t1 < t2 || (t1 == t2 && a1 <= a2)))

data BinaryTree3 v c = Node3 v c (BinaryTree3 v c) (BinaryTree3 v c )
  | Leaf3 deriving (Show, Eq, Ord)

-- | The 'treeInsert' function inserts and caches the min travelGuid
treeInsert3 :: (Ord v, Ord c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert3 v c Leaf3 = Node3 v c Leaf3 Leaf3
treeInserst3 v c n@(Node3 v2 c2 l r) = case compare v v2 of
  EQ -> n
  LT -> Node3 v2 (min c c2) (treeInsert3 v c l) r
  GT -> Node3 v2 (min c c2) l (treeInsert3 v c r)

-- | Binary Trees with Monoidal Cache
treeInsert4 :: (Ord v, Monoid c) => v -> c -> BinaryTree3 v c -> BinaryTree3 v c
treeInsert4 v c Leaf3               = Node3 v c Leaf3 Leaf3
treeInsert4 v c n@(Node3 v2 c2 l r) = case compare v v2 of
  EQ -> n
  LT ->
    let newLeft  = treeInsert4 v c l
        newCache = c2 <> (cached newLeft) <> (cached r)
    in  Node3 v2 newCache newLeft r
  GT ->
    let newRight = treeInsert4 v c r
        newCache = c2 <> (cached l) <> (cached newRight)
    in  Node3 v2 newCache l newRight


cached :: (Monoid c) => BinaryTree3 v c -> c
cached Leaf3           = mempty
cached (Node3 _ c _ _) = c

-- | Functors
modifyTravelGuidePrice :: Functor f => Double -> f TravelGuid -> f TravelGuid
modifyTravelGuidePrice m = fmap (\tg -> tg { price = m * price tg })
