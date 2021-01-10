{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase     #-}

-- |

module Chapter3 where

import           Data.List

-- ^ chapter3 oragami section

data Client i = GovOrg  { clientId :: i, clientName :: String }
              | Company { clientId :: i, clientName :: String
                         , person :: Person, duty :: String }
              | Individual { clientId :: i, person :: Person }
              deriving (Show, Eq, Ord)
                       -- Eq and Ord will be introduced in Chapter 4


data Person = Person { firstName :: String, lastName  :: String }
              deriving (Show, Eq, Ord)


filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold f = foldr (\x y -> if (f x) then x : y else y) []

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x y -> (f x) : y) []

minSort :: [Integer] -> [Integer]
minSort = unfoldr
  (\case
    [] -> Nothing
    xs -> Just (m, delete m xs) where m = minimum xs
  )
