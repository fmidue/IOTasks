{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Examples.Tasks where

import Prelude hiding (putStrLn, getLine, print)

import Test.IOTest.IOtt
import Test.IOTest.Language

import Test.QuickCheck as QC (Positive(..))

import Control.Monad (replicateM,replicateM_)

readFixedLengthList :: VarName -> NumType -> VarName -> Specification
readFixedLengthList n ty xs =
  TillE $
    Branch (MixedP (\ys m -> length ys == m) (xs,n)) (ReadInput xs ty) E

-- read natural number n, then read n integers and sum them
task1 :: Specification
task1 =
  ReadInput "n" NatTy <>
  readFixedLengthList "n" IntTy "xs" <>
  WriteOutput [UListF sum "xs"]

-- optional output of the first number read
task1' :: Specification
task1' =
  ReadInput "n" NatTy <>
  WriteOutput [Optional, UIntF id "n"] <>
  readFixedLengthList "n" IntTy "xs" <>
  WriteOutput [UListF sum "xs"]

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
  putStrLn $ show n
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

dList :: VarName -> ([Int] -> Bool) -> Specification
dList xs p =
  TillE $
    ReadInput xs IntTy <>
    Branch (UListP p xs)
      Nop
      E

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
task2 :: Specification
task2 =
  dList "xs" (\xs -> length xs > 1 && last xs + last (init xs) == 0) <>
  WriteOutput [UListF (\xs -> length [ x | x <- xs, x > 0, x `mod` 3 == 0]) "xs"]

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
  TillE $
    ReadInput "x" IntTy <>
    Branch (UIntP (0 ==) "x") Nop (WriteOutput [UListF sum "x"] <> E)

task3' :: Specification
task3' =
  dList "xs" (\xs -> last xs == 0) <>
  WriteOutput [UListF sum "xs"]

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
  ReadInput "x" IntTy <>
  (
    ReadInput "x" IntTy <>
    WriteOutput [UIntF id "x"]
  ) <>
  WriteOutput [UIntF id "x"]

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
printNSpec (QC.Positive 0)  _ = Nop
printNSpec (QC.Positive n) x =
  WriteOutput [Const x] <>
  printNSpec (QC.Positive $ n-1) x

printN :: Int -> Int -> IOtt ()
printN n x = replicateM_ n $ print x
