{-# LANGUAGE TypeApplications #-}
module ProductConditionSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

-- attempt at 'breaking' the solver
specification :: Specification
specification =
  readInput x nats AssumeValid <>
  whileNot (currentValue x .==. product' (allValues y))
    (readInput y nats AssumeValid)
  where
    x = intVar "x"
    y = intVar "y"

program :: MonadTeletype m => m ()
program = do
  x <- readLn @_ @Integer
  let
    loop p
      | p == x = pure ()
      | otherwise = do
        y <- readLn
        loop (p*y)
  loop 1

spec :: Spec
spec =
  describe "taskCheck program specification" $
    it "succeeds for small unfolding parameter" $
      (taskCheckWithOutcome stdArgs{maxIterationUnfold = 12, solverTimeout = 2000} program specification <&> isSuccess) `shouldReturn` True
