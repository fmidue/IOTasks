{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Examples.SampleTasks where

import Prelude hiding (putStrLn, getLine, readLn, print)

import Test.IOTasks
import Test.IOTasks.Term.ITerm (ITerm, lit)
import qualified Test.IOTasks.Term.ITerm.Prelude as T

import Control.Monad (replicateM,replicateM_)

import Test.QuickCheck as QC (Positive(..))

-- Example 1:
-- read natural number n, then read n integers and sum them
ex1 :: Specification ITerm
ex1 =
  readInput "n" (intValues [0..10]) <>
  tillExit (
    branch ( T.length (getAll @Int "xs") T.== getCurrent "n")
     ( readInput "xs" (intValues [-10..10]) )
     exit
  ) <>
  writeOutput ["#0"] [T.sum $ getAll @Int "xs"]

ex1Combinators :: Specification ITerm
ex1Combinators =
  optional (writeFixedOutput ["_"]) <>
  readInput "n" nats <>
  optional (writeOutput ["_#0_"] [getCurrent @Int "n"]) <>
  readTillFixedLength @Int T.length (T.==) "n" ints "xs" <>
  writeOutput ["_#0_"] [T.sum $ getAll @Int "xs"]

solution1 :: IOrep ()
solution1 = do
  n <- readLn
  let loop xs =
        if length xs == n
          then print (sum xs)
          else do
            v <- readLn
            loop (xs ++ [v])
  loop []

-- With possible extra outputs
ex1Pattern :: Specification ITerm
ex1Pattern =
  writeFixedOutput ["_"] <>
  readInput "n" nats <>
  tillExit (
    branch (T.length (getAll @Int "xs") T.== getCurrent "n")
     ( optional (writeOutput ["_#0_"] [T.length (getAll @Int "xs")]) <>
       readInput "xs" ints
     )
     exit
  ) <>
  writeOutput ["_#0_"] [T.sum $ getAll @Int "xs"]

ex1PatternCombinators :: Specification ITerm
ex1PatternCombinators =
  writeFixedOutput ["_"] <>
  readInput "n" nats <>
  readTillFixedLength @Int T.length (T.==) "n" ints "xs" <>
  writeOutput ["#0"] [T.sum $ getAll @Int "xs"]

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
ex2 :: Specification ITerm
ex2 =
  repeatSpec 2 (readInput "xs" ints) <> --otherwise the condition will throw an exception
  readUntil "xs" (let xs = getAll "xs" in T.length xs T.> lit 1 T.&& (T.last xs T.+ T.last (T.init xs) T.== lit (0 :: Int)) ) ints <>
  writeOutput ["_#0_"] [count $ getAll @Int "xs"]
  where count xs = T.length $ T.filter (\x -> x T.> lit 0 T.&& (x `T.mod` lit 3 T.== lit 0)) xs

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
ex3 :: Specification ITerm
ex3 =
  tillExit $
    readInput "x" ints <>
    when (lit 0 T.== getCurrent @Int "x")
      (writeOutput ["_#0_"] [T.sum $ getAll @Int "x"] <> exit)

ex3Combinators :: Specification ITerm
ex3Combinators =
  readInput "xs" ints <> --otherwise last will fail
  readUntil "xs" (T.last (getAll @Int "xs") T.== lit 0) ints <>
  writeOutput ["_#0_"] [T.sum $ getAll @Int "xs"]

solution3 :: IOrep ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n Prelude.== 0
      then print $ Prelude.sum xs
      else go $ n:xs

-- Example 4:
-- read and reverse
ex4 ::Specification ITerm
ex4 =
  readInput "line" (stringValues ("_" :: Pattern)) <>
  writeOutput ["_#0_"] [T.reverse $ getCurrent @String "line"]

solution4 :: IOrep ()
solution4 = (Prelude.reverse <$> getLine) >>= putStrLn

wrongSolution4 :: IOrep ()
wrongSolution4 = getLine >>= putStrLn

-- Example 5:
-- specificing parameterized tasks/programs
printNSpec :: QC.Positive Int -> Int -> Specification ITerm
printNSpec (QC.Positive n) x = repeatSpec n $ writeFixedOutput [buildTermPattern (show x)]

printN :: Int -> Int -> IOrep ()
printN n x = replicateM_ n $ print x
