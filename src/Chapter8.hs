{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Chapter8, working in several cores
module Chapter8 where

import qualified Control.Concurrent.STM as STM
import qualified Control.DeepSeq as D
import qualified Control.Monad.Par as P

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n =
  let oneFactor = findFactor n 2
   in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m
  | n == m = n
  | n `mod` m == 0 = m
  | otherwise = findFactor n (m + 1)

findTwoFactors,
  findTwoFactorsP ::
    Integer -> Integer -> ([Integer], [Integer])
findTwoFactors x y = (findFactors x, findFactors y)
findTwoFactorsP x y = P.runPar $ do
  factorsXVar <- P.spawnP $ findFactors x
  let factorsY = findFactors y
      _ = D.rnf factorsY
  factorsX <- P.get factorsXVar
  return (factorsX, factorsY)

printTicket :: Int -> Int -> [(Int, String)] -> [(Int, String)] -> String
printTicket idC idP clients products =
  P.runPar $ do
    clientV <- P.new
    productV <- P.new
    P.fork $ lookupPar clientV idC clients
    P.fork $ lookupPar productV idP products
    envV <- P.new
    letterV <- P.new
    P.fork $ printEnvelop clientV envV
    P.fork $ printLetter clientV productV letterV
    envS <- P.get envV
    letterS <- P.get letterV
    return $ envS ++ "\n\n" ++ letterS

lookupPar :: (Eq a, D.NFData b) => P.IVar (Maybe b) -> a -> [(a, b)] -> P.Par ()
lookupPar i _ [] = P.put i Nothing
lookupPar i x ((k, v) : r)
  | x == k = P.put i $ Just v
  | otherwise = lookupPar i x r

printEnvelop :: P.IVar (Maybe String) -> P.IVar String -> P.Par ()
printEnvelop clientV envV = do
  clientName <- P.get clientV
  case clientName of
    Nothing -> P.put envV "Unkown"
    Just c -> P.put envV $ "To: " ++ c

printLetter ::
  P.IVar (Maybe String) ->
  P.IVar (Maybe String) ->
  P.IVar String ->
  P.Par ()
printLetter clientV productV letterV =
  do
    clientName <- P.get clientV
    productName <- P.get productV
    case (clientName, productName) of
      (Nothing, Nothing) -> P.put letterV "Unkown"
      (Just n, Nothing) -> P.put letterV $ n ++ "bought something"
      (Nothing, Just p) -> P.put letterV $ "Someone bought " ++ p
      (Just n, Just p) -> P.put letterV $ n ++ "bought " ++ p

updateMoneyAndStackStm ::
  Eq a =>
  a ->
  Integer ->
  STM.TVar Integer ->
  STM.TVar [(a, Integer)] ->
  STM.STM ()
updateMoneyAndStackStm product price money stock =
  do
    s <- STM.readTVar stock
    let Just productNo = lookup product s
    if productNo > 0
      then do
        m <- STM.readTVar money
        let newS =
              map
                ( \(k, v) ->
                    if k == product
                      then (k, v -1)
                      else (k, v)
                )
                s
        STM.writeTVar money (m + price) >> STM.writeTVar stock newS
      else return ()

payByCard :: Eq a => a -> Integer -> STM.TVar Integer -> STM.TVar [(a, Integer)] -> STM.STM ()
payByCard product price money stock =
  do
    working <- isCardSystemWorking
    if not working
      then STM.retry
      else updateMoneyAndStackStm product price money stock

isCardSystemWorking :: STM.STM Bool
isCardSystemWorking = return True
