{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Examples.Examples where

import Prelude hiding (putStrLn, getLine, print)

import Test.IOTest.IOrep
import Test.IOTest.Language
import Test.IOTest.Combinators
import Test.IOTest.ValueSet

import Control.Monad (replicateM,replicateM_)

import Data.Maybe

import Test.QuickCheck as QC (Positive(..))
import Text.Read (readMaybe)

nats :: ValueSet
nats = intValues [0..10]

ints :: ValueSet
ints = intValues [-10..10]

-- read natural number n, then read n integers and sum them
task1 :: Specification
task1 =
  writeFixedOutput ["_"] <>
  readInput "n" (intValues [0..10]) <>
  readTillFixedLength "n" (intValues [-10..10]) "xs" <>
  writeOutput ["#0"] [sum <$> getAll @Int "xs"]

spec :: Specification
spec =
  writeFixedOutput ["_"] <>
  readInput "n" (intValues [0..10]) <>
  tillExit (
    branch ((\n xs -> length xs == n) <$> getCurrent "n" <*> getAll @Int "xs")
     ( writeOutput ["_#0_"] [length <$> getAll @Int "xs"] <>
       readInput "xs" (intValues [-10..10])
     )
     exit
  ) <>
  writeOutput ["_#0_"] [sum <$> getAll @Int "xs"]

task1' :: Specification
task1' =
  optional (writeFixedOutput ["_"]) <>
  readInput "n" nats <>
  optional (writeOutput ["_#0_"] [getCurrent @Int "n"]) <>
  readTillFixedLength "n" ints "xs" <>
  writeOutput ["_#0_"] [sum <$> getAll @Int "xs"]

solution1 :: IOrep ()
solution1 = do
  putStrLn "> "
  n <- read @Int <$> getLine
  xs <- replicateM n $ read @Int <$> getLine
  print $ sum xs

solution1' :: IOrep ()
solution1' = do
  putStrLn "> "
  n <- read @Int <$> getLine
  putStrLn $ "You entered " ++ show n
  xs <- replicateM n $ read @Int <$> getLine
  putStrLn "Result: "
  print $ sum xs

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
  readUntil "xs" ((\xs -> length xs > 1 && last xs + last (init xs) == 0 ) <$> getAll @Int "xs") ints <>
  writeOutput ["_#0_"] [count <$> getAll @Int "xs"]
  where count xs = length [ x | x <- xs, x > 0, x `mod` 3 == 0]

solution2 :: IOrep ()
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
  tillExit $
    readInput "x" ints <>
    when ((0==) <$> getCurrent @Int "x")
      (writeOutput ["_#0_"] [sum <$> getAll @Int "x"] <> exit)

task3' :: Specification
task3' =
  readInput "xs" ints <> --otherwise last will fail
  readUntil "xs" ((\xs -> last xs == 0) <$> getAll @Int "xs") ints <>
  writeOutput ["_#0_"] [sum <$> getAll @Int "xs"]

solution3 :: IOrep ()
solution3 = go [] where
  go xs = do
    n <- read @Int <$> getLine
    if n == 0
      then print $ sum xs
      else go $ n:xs

-- read and reverse
task4 :: Specification
task4 =
  readInput "line" (valueSet ("_" :: Pattern)) <>
  writeOutput ["_#0_"] [reverse <$> getCurrentS "line"]

solution4 :: IOrep ()
solution4 = (reverse <$> getLine) >>= putStrLn

wrongSolution4 :: IOrep ()
wrongSolution4 = getLine >>= putStrLn

scoping :: Specification
scoping =
  readInput "x" ints <>
  (
    readInput "x" ints <>
    writeOutput ["#0"] [getCurrent @Int "x"]
  ) <>
  writeOutput ["#0"] [getCurrent @Int "x"]

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

parseSumSpec :: Specification
parseSumSpec =
  tillExit (
    readInput "line" (valueSet ((show @Int <$> [1..10]) ++ (show <$> ['a'..'k']) ) ) <>
    when ((==2) . length . filter isJust . fmap (readMaybe @Int) <$> getAllS "line") exit
  ) <>
  writeOutput ["#0"] [sum . fmap fromJust . filter isJust . fmap (readMaybe @Int) <$> getAllS "line"]

parseSum :: IOrep ()
parseSum = do
  let go =
        do mInt <- readMaybe @Int <$> getLine
           case mInt of
             Just n -> return n
             Nothing -> go
  x <- go
  y <- go
  print $ x + y
