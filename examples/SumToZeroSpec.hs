{-# LANGUAGE TypeApplications #-}
module SumToZeroSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

-- sum to 0
sumToZeroSpec :: Specification
sumToZeroSpec =
  readInput x allInts AssumeValid <>
  tillExit (
    readInput x allInts AssumeValid <>
    branch (valueBefore 1 x .+. currentValue x .==. intLit 0)
      exit
      nop
    ) <>
  writeOutput [resultOf $ length' $ allValues x]
  where x = intVar "x"

sumToZeroProg :: MonadTeletype m => m ()
sumToZeroProg = do
  x <- readLn @_ @Integer
  go x 1
  where
    go x n = do
      y <- readLn @_ @Integer
      let n' = n+1
      if x + y == 0
        then print n'
        else go y n'

-- verbose version

verboseSpec :: Specification
verboseSpec =
  readX <>
  whileNot (currentValue x .==. intLit 0)
    (readY <>
     writeOutput [wildcard <> resultOf (currentValue x .+. currentValue y) <> wildcard] <>
     readX
    ) <>
  writeOutput [wildcard <> resultOf (length' $ allValues y) <> wildcard]
  where
    readX =
      anyOptionalOutput <>
      readInput x allInts AssumeValid

    readY =
      anyOptionalOutput <>
      readInput y allInts AssumeValid

    x = intVar "x"
    y = intVar "y"

verboseProg :: MonadTeletype io => io ()
verboseProg = go 0
  where
    go :: MonadTeletype io => Integer -> io ()
    go n = do
      putStr "First number or 0 to exit: "
      x <- readLn
      if x == 0
        then do
          putStrLn "Exiting program"
          putStr "The number of additions performed was: "
          print n
        else do
          putStr "Second number: "
          y <- readLn
          putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is: ")
          print (x + y)
          go (n + 1)

spec :: Spec
spec = do
  describe "taskCheck sumToZeroProg sumToZeroSpec" $
    it "succeeds" $
      (taskCheckOutcome sumToZeroProg sumToZeroSpec <&> isSuccess) `shouldReturn` True
  describe "taskCheck sumToZeroProg verboseSpec" $
    it "succeeds" $
      (taskCheckOutcome verboseProg verboseSpec <&> isSuccess) `shouldReturn` True
  describe "taskCheck verboseProg verboseSpec" $
    it "succeeds" $
      (taskCheckOutcome verboseProg verboseSpec <&> isSuccess) `shouldReturn` True
