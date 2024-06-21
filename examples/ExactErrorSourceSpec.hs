{-# LANGUAGE TypeApplications #-}
module ExactErrorSourceSpec where

import Prelude hiding (readLn, putStrLn)
import Test.IOTasks

import Test.Hspec
import Data.Functor ((<&>))
import Test.IOTasks.Trace

specification :: Specification
specification =
  anyOptionalOutput <>
  readInput (intVar "x") ints AssumeValid <>
  writeOutput [text "A"]

main :: MonadTeletype io => io ()
main = do
  _ <- readLn @_ @Integer
  putStrLn "B"

spec :: Spec
spec =
  describe "spurious errors due to optionality" $
    it "should not shadow real errors" $
      (taskCheckOutcome main specification <&> failureResultSatisfies isOutputMismatch) `shouldReturn` True

failureResultSatisfies :: (MatchResult -> Bool) -> Outcome -> Bool
failureResultSatisfies p (Outcome (Failure _ _ _ result) _) = p result
failureResultSatisfies _ _ = False
