{-# LANGUAGE TypeApplications #-}
module Examples.Tasks where

import Prelude hiding (putStrLn, getLine, print)

import Interaction
import IOtt
import Types

import Control.Monad (replicateM,replicateM_)

-- read natural number n, then read n integers and sum them
task1 :: Spec
task1 = StepSpecs
  [ (In ("n",natTy)       , prompt "> "
                            <& mayReactWith (\v -> MatchExactly $ "You entered "++ show v))
  , (In ("xs", SListTy intTy "n"), doNothing)
  , (Out (sumOf,["xs"])   , displayWithPrefix "Result:")
  ]

solution1 :: IOtt ()
solution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn $ "Result: " ++ show (sum xs)
  --print $ sum xs

wrongSolution1 :: IOtt ()
wrongSolution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  putStrLn $ "You entered " ++ show n
  replicateM_ n $ read @Int <$> getLine
  putStrLn "Result: 17"


-- read till last two numbers sum to 0 than count positive numbers divisible by 3
task2 :: Spec
task2 = StepSpecs
  [ (In ("xs", DListTy intTy [("x",intTy),("y",neg "x")]), doNothing)
  , (Out (count (\(IntVal x) ->  x > 0 && x `mod` 3 == 0), ["xs"]), displayValue)
  ]

solution2 :: IOtt ()
solution2 = go [] Nothing Nothing where
  go ns mX mY =
    if ((+) <$> mX <*> mY) == Just 0
      then
        print $ length [ x | x <- ns, x > 0, x `mod` 3 == 0 ]
      else do
        n <- read @Int <$> getLine
        go (n:ns) (Just n) mX

-- read till zero then sum
task3 :: Spec
task3 = StepSpecs
  [ (In ("xs", DListTy intTy [("",exact 0)]), doNothing)
  , (Out (sumOf, ["xs"]), displayValue)
  ]

solution3 :: IOtt ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n == 0
      then print $ sum xs
      else go $ n:xs

-- read and reverse
task4 :: Spec
task4 = StepSpecs
  [ (In ("line", line), doNothing)
  , (Out (reverseLine, ["line"]) , displayValue)
  ]

solution4 :: IOtt ()
solution4 = (reverse <$> getLine) >>= putStrLn

wrongSolution4 :: IOtt ()
wrongSolution4 = getLine >>= putStrLn
