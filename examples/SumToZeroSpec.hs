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
  readInput x ints AssumeValid <>
  tillExit (
    readInput x ints AssumeValid <>
    branch (valueBefore 1 x .+. currentValue x .==. intLit 0)
      exit
      nop
    ) <>
  writeOutput [resultOf $ length' $ as @[Integer] $ allValues x]
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
        then print $ n'
        else go y n'

spec :: Spec
spec =
  describe "taskCheck sumToZeroProg sumToZeroSpec" $
    it "succeeds" $
      (taskCheckOutcome sumToZeroProg sumToZeroSpec <&> isSuccess) `shouldReturn` True
