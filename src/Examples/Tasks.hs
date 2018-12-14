{-# LANGUAGE TypeApplications #-}
module Examples.Tasks where

import Prelude hiding (putStrLn, getLine, print)

import Interaction
import IOtt
import Types

import Control.Monad (replicateM)

-- read natural number n, then read n integers and sum them
task1 :: Spec
task1 = StepSpecs
  [ (In ("n",NatTy)       , prompt "> "
                            <& mayReactWith (\v -> MatchExactly $ "You entered "++ show v))
  , (In ("xs", SListTy IntTy "n"), doNothing)
  , (Out (sumOf,["xs"])   , displayWithPrefix "Result:")
  ]

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
task2 :: Spec
task2 = StepSpecs
  [ (In ("xs", DListTy IntTy [("x",IntTy),("y",Neg "x")]), doNothing)
  , (Out (count (\(IntVal x) ->  x > 0 && x `mod` 3 == 0), ["xs"]), displayValue)
  ]

-- read till zero then sum
task3 :: Spec
task3 = StepSpecs
  [ (In ("xs", DListTy IntTy [("",Exact 0)]), doNothing)
  , (Out (sumOf, ["xs"]), displayValue)
  ]

solution1 :: IOtt ()
solution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn $ "Result: " ++ show (sum xs)
  --print $ sum xs

solution2 :: IOtt ()
solution2 = go [] Nothing Nothing where
  go ns mX mY =
    if ((+) <$> mX <*> mY) == Just 0
      then
        print $ length [ x | x <- ns, x > 0, x `mod` 3 == 0 ]
      else do
        n <- read @Int <$> getLine
        go (n:ns) (Just n) mX

solution3 :: IOtt ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n == 0
      then print $ sum xs
      else go $ n:xs
