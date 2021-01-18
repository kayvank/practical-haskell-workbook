{-# LANGUAGE ScopedTypeVariables #-}

-- |
module Chapter9 where

listOfActions :: [(String, IO ())]
listOfActions =
  [ ( "geeting",
      do
        name <- getLine
        putStrLn $ "Hello " ++ name
    ),
    ( "sum",
      do
        putStrLn "First Number: "
        n1 :: Integer <- fmap read getLine
        putStrLn "Second Number: "
        n2 :: Integer <- fmap read getLine
        putStrLn $ show n1 ++ "+" ++ show n2 ++ "=" ++ show (n1 + n2)
    )
  ]

c9main :: IO ()
c9main = do
  actionName <- getLine
  case lookup actionName listOfActions of
    Just action -> action
    Nothing -> putStrLn "Unknown action"
