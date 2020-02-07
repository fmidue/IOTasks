{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Examples.Examples where

import Prelude hiding (putStrLn, getLine, print)

import Test.IOTest
import Test.IOTest.IOrep
import Test.IOTest.Combinators
import Test.IOTest.ValueSet
import Test.IOTest.Term as T

import Control.Monad (replicateM,replicateM_)

import Data.Maybe

import Test.QuickCheck as QC (Positive(..))
import Text.Read (readMaybe)

-- read natural number n, then read n integers and sum them
task1 :: Specification
task1 =
  writeFixedOutput ["_"] <>
  readInput "n" nats <>
  readTillFixedLength "n" ints "xs" <>
  writeOutput ["#0"] [T.sum $ T.getAll @Int "xs"]

spec :: Specification
spec =
  writeFixedOutput ["_"] <>
  readInput "n" nats <>
  tillExit (
    branch (T.length (T.getAll @Int "xs") T.== T.getCurrent "n")
     ( writeOutput ["_#0_"] [T.length (T.getAll @Int "xs")] <>
       readInput "xs" ints
     )
     exit
  ) <>
  writeOutput ["_#0_"] [T.sum $ T.getAll @Int "xs"]

spec' :: Specification
spec' =
  readInput "n" (intValues [0..10]) <>
  tillExit (
    branch ( T.length (T.getAll @Int "xs") T.== T.getCurrent "n")
     ( readInput "xs" (intValues [-10..10]) )
     exit
  ) <>
  writeOutput ["#0"] [T.sum $ T.getAll @Int "xs"]

task1' :: Specification
task1' =
  optional (writeFixedOutput ["_"]) <>
  readInput "n" nats <>
  optional (writeOutput ["_#0_"] [T.getCurrent @Int "n"]) <>
  readTillFixedLength "n" ints "xs" <>
  writeOutput ["_#0_"] [T.sum $ T.getAll @Int "xs"]

solution1 :: IOrep ()
solution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  xs <- replicateM n $ read @Int <$> getLine
  print $ Prelude.sum xs

solution1' :: IOrep ()
solution1' = do
  putStrLn "> "
  n <- read @Int <$> getLine
  putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn "Result: "
  print $ Prelude.sum xs

wrongSolution1 :: IOrep ()
wrongSolution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  --putStrLn $ "You entered " ++ show n
  replicateM_ n $ read @Int <$> getLine
  putStrLn "17"

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
task2 :: Specification
task2 =
  repeatSpec 2 (readInput "xs" ints) <> --otherwise the condition will throw an exception
  readUntil "xs" (let xs = T.getAll "xs" in T.length xs T.> lit 1 T.&& (T.last xs T.+ T.last (T.init xs) T.== lit (0 :: Int)) ) ints <>
  writeOutput ["_#0_"] [count $ T.getAll @Int "xs"]
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

-- read till zero then sum
task3 :: Specification
task3 =
  tillExit $
    readInput "x" ints <>
    when (lit 0 T.== T.getCurrent @Int "x")
      (writeOutput ["_#0_"] [T.sum $ T.getAll @Int "x"] <> exit)

task3' :: Specification
task3' =
  readInput "xs" ints <> --otherwise last will fail
  readUntil "xs" (T.last (T.getAll @Int "xs") T.== lit 0) ints <>
  writeOutput ["_#0_"] [T.sum $ T.getAll @Int "xs"]

solution3 :: IOrep ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n Prelude.== 0
      then print $ Prelude.sum xs
      else go $ n:xs

-- read and reverse
task4 ::Specification
task4 =
  readInput "line" (valueSet ("_" :: Pattern)) <>
  writeOutput ["_#0_"] [T.reverse $ T.getCurrent @String "line"]

solution4 :: IOrep ()
solution4 = (Prelude.reverse <$> getLine) >>= putStrLn

wrongSolution4 :: IOrep ()
wrongSolution4 = getLine >>= putStrLn

scoping :: Specification
scoping =
  readInput "x" ints <>
  (
    readInput "x" ints <>
    writeOutput ["#0"] [T.getCurrent @Int "x"]
  ) <>
  writeOutput ["#0"] [T.getCurrent @Int "x"]

scopingRight :: IOrep ()
scopingRight = do
  _x <- read @Int <$> getLine
  x <- read @Int <$> getLine
  print x
  print x

scopingWrong :: IOrep ()
scopingWrong = do
  x <- read @Int <$> getLine
  y <- read @Int <$> getLine
  print y
  print x

printNSpec :: QC.Positive Int -> Int -> Specification
printNSpec (QC.Positive n) x = repeatSpec n $ writeFixedOutput [buildTermPattern (show x)]

printN :: Int -> Int -> IOrep ()
printN n x = replicateM_ n $ print x

-- parseSumSpec :: (Term t, Functor t) => Specification t
-- parseSumSpec =
--   tillExit (
--     readInput "line" (valueSet ((show @Int <$> [1..10]) ++ (show <$> ['a'..'k']) ) ) <>
--     when ((==2) . length . filter isJust . fmap (readMaybe @Int) <$> getAll "line") exit
--   ) <>
--   writeOutput ["#0"] [sum . fmap fromJust . filter isJust . fmap (readMaybe @Int) <$> getAll "line"]
--
-- parseSum :: IOrep ()
-- parseSum = do
--   let go =
--         do mInt <- readMaybe @Int <$> getLine
--            case mInt of
--              Just n -> return n
--              Nothing -> go
--   x <- go
--   y <- go
--   print $ x + y
