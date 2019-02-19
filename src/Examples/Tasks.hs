{-# LANGUAGE TypeApplications #-}
module Examples.Tasks where

import Prelude hiding (putStrLn, getLine, print)

import IOtt
import Language

import Control.Monad (replicateM,replicateM_)

readFixedLengthList :: VarName -> NumType -> VarName -> Specification
readFixedLengthList n ty xs =
  TillT $
    ReadInput "<doNotGuessThis>" ty xs <>
    Branch (MixedP (\ys m -> length ys == m) (xs,n)) Nop T

-- read natural number n, then read n integers and sum them
task1 :: Specification
task1 =
  ReadInput "n" Positive "" <> -- Positive should be Nat, but that does not work with readFixedLengthList since at least one read is performed
  readFixedLengthList "n" IntTy "xs" <>
  WriteOutput [UListF sum "xs"]

-- optional output of the first number read
task1' :: Specification
task1' =
  ReadInput "n" Positive "" <>
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
  TillT $
    ReadInput "x" IntTy xs <>
    Branch (UListP p xs)
      Nop
      T

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
  TillT $
    ReadInput "x" IntTy "xs" <>
    Branch (UIntP (0 ==) "x") Nop (WriteOutput [UListF sum "xs"] <> T)

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
