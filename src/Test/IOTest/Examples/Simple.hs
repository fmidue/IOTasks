{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Examples.Simple where

import Prelude hiding (putStrLn, getLine, print)

import Test.IOTest.IOtt
import Test.IOTest.Simple.Language
import Test.IOTest.Simple.Combinators

import Test.QuickCheck as QC (Positive(..))

import Control.Monad (replicateM,replicateM_)

-- read natural number n, then read n integers and sum them
task1 :: Specification
task1 =
  readInput "n" NatTy <>
  readTillFixedLength "n" IntTy "xs" <>
  writeOutput [sum <$> getAll "xs"]

-- optional output of the first number read
task1' :: Specification
task1' =
  readInput "n" NatTy <>
  readTillFixedLength "n" IntTy "xs" <>
  writeOutput [sum <$> getAll "xs"]

solution1 :: IOtt ()
solution1 = do
  --putStrLn "> "
  n <- read @Int <$> getLine
  --putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  --putStrLn $ "Result: " ++ show (sum xs)
  putStrLn $ show (sum xs)
  --print $ sum xs

solution1' :: IOtt ()
solution1' = do
  n <- read @Int <$> getLine
  --putStrLn $ show n
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn $ show (sum xs)

wrongSolution1 :: IOtt ()
wrongSolution1 = do
  --putStrLn "> "
  n <- read @Int <$> getLine
  --putStrLn $ "You entered " ++ show n
  replicateM_ n $ read @Int <$> getLine
  putStrLn "17"
  --putStrLn "Result: 17"

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
task2 :: Specification
task2 =
  readUntil1 "xs" (\xs -> length xs > 1 && last xs + last (init xs) == 0) IntTy <>
  writeOutput [count <$> getAll "xs"]
  where count xs = length [ x | x <- xs, x > 0, x `mod` 3 == 0]

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
task3 :: Specification
task3 =
  tillE $
    readInput "x" IntTy <>
    when ((0==) <$> getCurrent "x")
      (writeOutput [sum <$> getAll "x"] <> e)

task3' :: Specification
task3' =
  readUntil1 "xs" (\xs -> last xs == 0) IntTy <>
  writeOutput [sum <$> getAll "xs"]

solution3 :: IOtt ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n == 0
      then print $ sum xs
      else go $ n:xs
--
-- -- read and reverse
-- task4 :: Spec
-- task4 = StepSpecs
--   [ (In ("line", line), doNothing)
--   , (Out (reverseLine, ["line"]) , displayValue)
--   ]
--
-- solution4 :: IOtt ()
-- solution4 = (reverse <$> getLine) >>= putStrLn
--
-- wrongSolution4 :: IOtt ()
-- wrongSolution4 = getLine >>= putStrLn

scoping :: Specification
scoping =
  readInput "x" IntTy <>
  (
    readInput "x" IntTy <>
    writeOutput [getCurrent "x"]
  ) <>
  writeOutput [getCurrent "x"]

scopingRight :: IOtt ()
scopingRight = do
  _x <- read @Int <$> getLine
  x <- read @Int <$> getLine
  print x
  print x

scopingWrong :: IOtt ()
scopingWrong = do
  x <- read @Int <$> getLine
  y <- read @Int <$> getLine
  print y
  print x

printNSpec :: QC.Positive Int -> Int -> Specification
printNSpec (QC.Positive n) x = repeatSpec n $ writeOutput [pure x]

printN :: Int -> Int -> IOtt ()
printN n x = replicateM_ n $ print x
