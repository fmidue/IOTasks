{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Examples.SampleTasks where

import Prelude hiding (putStrLn, getLine, readLn, print, putStr)

import Test.IOTasks hiding (SpecTerm)

import Data.Term.Typed.AST (AST)
import Data.Term.Liftable (litT, liftT)
import qualified Data.Term.Liftable.Prelude as T

import Control.Monad (replicateM,replicateM_)

import Test.QuickCheck as QC (Positive(..))

type SpecTerm = AST Varname

-- Example 1:
-- read natural number n, then read n integers and sum them
ex1 :: Specification SpecTerm
ex1 =
  readInput "n" (intValues [0..10]) <>
  tillExit (
    branch ( T.length (getAll @Int "xs") T.== getCurrent "n")
     ( readInput "xs" (intValues [-10..10]) )
     exit
  ) <>
  writeOutput [var 0] [T.sum $ getAll @Int "xs"]

ex1' :: Specification SpecTerm
ex1' =
  readInput "n" (intValues [0..10]) <>
  tillExit (
    branch ( T.length (getAll @Int "xs") T.> getCurrent "n")
     ( readInput "xs" (intValues [-10..10]) <>
       readInput "xs" (intValues [-10..10]) )
     exit
  ) <>
  writeOutput [var 0] [T.sum $ getAll @Int "xs"]

ex1Combinators :: Specification SpecTerm
ex1Combinators =
  optional (writeFixedOutput [anything]) <>
  readInput "n" nats <>
  optional (writeOutput [anything <> var 0 <> anything] [getCurrent @Int "n"]) <>
  readTillFixedLength @Int T.length (T.==) "n" ints "xs" <>
  writeOutput [anything <> var 0 <> anything] [T.sum $ getAll @Int "xs"]

solution1 :: IOrep ()
solution1 = do
  n <- readLn
  let loop xs =
        if length xs == n
          then putStr (show $ sum xs)
          else do
            v <- readLn
            loop (xs ++ [v])
  loop []

-- With possible extra outputs
ex1Pattern :: Specification SpecTerm
ex1Pattern =
  writeFixedOutput [anything] <>
  readInput "n" nats <>
  tillExit (
    branch (T.length (getAll @Int "xs") T.== getCurrent "n")
     ( optional (writeOutput [anything <> var 0 <> anything] [T.length (getAll @Int "xs")]) <>
       readInput "xs" ints
     )
     exit
  ) <>
  writeOutput [anything <> var 0 <> anything] [T.sum $ getAll @Int "xs"]

ex1PatternCombinators :: Specification SpecTerm
ex1PatternCombinators =
  writeFixedOutput [anything] <>
  readInput "n" nats <>
  readTillFixedLength @Int T.length (T.==) "n" ints "xs" <>
  writeOutput [anything <> var 0 <> anything] [T.sum $ getAll @Int "xs"]

solution1Pat :: IOrep ()
solution1Pat = do
  putStrLn "> "
  n <- read @Int <$> getLine
  xs <- replicateM n $ read @Int <$> getLine
  print $ Prelude.sum xs

solution1Pat' :: IOrep ()
solution1Pat' = do
  putStrLn "> "
  n <- read @Int <$> getLine
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn "Result: "
  print $ Prelude.sum xs

wrongSolutionPat1 :: IOrep ()
wrongSolutionPat1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  replicateM_ n $ read @Int <$> getLine
  putStrLn "17"

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
ex2 :: Specification SpecTerm
ex2 =
  repeatSpec 2 (readInput "xs" ints) <> --otherwise the condition will throw an exception
  readUntil "xs" (let xs = getAll "xs" in T.length xs T.> litT 1 T.&& (T.last xs T.+ T.last (T.init xs) T.== litT (0 :: Int)) ) ints <>
  writeOutput [anything <> var 0 <> anything] [count $ getAll @Int "xs"]
  where count xs = T.length $ T.filter (\x -> x T.> litT 0 T.&& (x `T.mod` litT 3 T.== litT 0)) xs

solution2 :: IOrep ()
solution2 = go [] Nothing Nothing where
  go ns mX mY =
    if ((Prelude.+) <$> mX <*> mY) Prelude.== Just 0
      then
        print $ Prelude.length [ x | x <- ns, x Prelude.> 0, x `Prelude.mod` 3 Prelude.== 0 ]
      else do
        n <- read @Int <$> getLine
        go (n:ns) (Just n) mX

-- Example 3:
-- read till zero then sum
ex3 :: Specification SpecTerm
ex3 =
  tillExit $
    readInput "x" ints <>
    when (litT 0 T.== getCurrent @Int "x")
      (writeOutput [anything <> var 0 <> anything] [T.sum $ getAll @Int "x"] <> exit)

ex3Combinators :: Specification SpecTerm
ex3Combinators =
  readInput "xs" ints <> --otherwise last will fail
  readUntil "xs" (T.last (getAll @Int "xs") T.== litT 0) ints <>
  writeOutput [anything <> var 0 <> anything] [T.sum $ getAll @Int "xs"]

solution3 :: IOrep ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n Prelude.== 0
      then print $ Prelude.sum xs
      else go $ n:xs

-- Example 4:
-- read and reverse
ex4 ::Specification SpecTerm
ex4 =
  readInput "line" (stringValues (anything :: FixedPattern)) <>
  writeOutput [anything <> var 0 <> anything] [T.reverse $ getCurrent @String "line"]

solution4 :: IOrep ()
solution4 = (Prelude.reverse <$> getLine) >>= putStrLn

wrongSolution4 :: IOrep ()
wrongSolution4 = getLine >>= putStrLn

-- Example 5:
-- specificing parameterized tasks/programs
printNSpec :: QC.Positive Int -> Int -> Specification SpecTerm
printNSpec (QC.Positive n) x = repeatSpec n $ writeFixedOutput [text (show x) <> linebreak]

printN :: Int -> Int -> IOrep ()
printN n x = replicateM_ n $ print x

-- Example 6:
-- 'local state' programs
printSequenceSpec :: Specification SpecTerm
printSequenceSpec =
  readInput "x" (values [1 :: Int .. 9])
  -- this way of lifting, obviously, hides the definition of output
  <> writeOutput [var 0] [liftT (unlines . output,"output") $ getCurrent @Int "x"]
  where
    output :: Int -> [String]
    output 1 = ["1"]
    output x = show x : if even x then output (x `div` 2) else output (x + 1)

printSequence :: IOrep ()
printSequence = do
  v <- readLn
  let loop 1 = print 1
      loop x = do
        print x
        if even x
          then loop (x `div` 2)
          else loop (x + 1)
  loop v
