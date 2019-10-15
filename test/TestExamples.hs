{-# LANGUAGE TypeApplications #-}
module TestExamples where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOrep
import Test.IOTest.IOProperty
import Test.IOTest.Translation
import Test.IOTest.Examples.Examples

import Test.Hspec
import Test.Hspec.QuickCheck (prop, modifyMaxSize)
import Test.QuickCheck

import SpecGen

testExamples :: Spec
testExamples = describe "Testing Test.IOTest.Examples.Examples:" $ do
  prop "solution1 matches task1" $
    solution1 `fulfills` task1
  prop "solution1' does not match task1" $
    solution1' `fulfillsNot` task1
  prop "wrongSolution1 does not match task1" $
    wrongSolution1 `fulfillsNot` task1
  prop "solution1 matches task1'" $
    solution1 `fulfills` task1'

  prop "solution1' matches task1'" $
    solution1' `fulfills` task1'
  prop "solution2 matches task2" $
   solution2 `fulfills` task2
  prop "solution3 matches task3" $
    solution3 `fulfills` task3
  prop "solution3 matches task3'" $
    solution3 `fulfills` task3'

  prop "program generated from task1 matches task1" $
    buildComputation @IOrep task1 `fulfills` task1
  prop "program generated from task1' matches task1'" $
    buildComputation @IOrep task1' `fulfills` task1'
  prop "program generated from task2 matches task2" $
    buildComputation @IOrep task2 `fulfills` task2
  prop "program generated from task3 matches task3" $
    buildComputation @IOrep task3 `fulfills` task3
  prop "program generated from task3' matches task3'" $
    buildComputation @IOrep task3' `fulfills` task3'

  prop "Testing solution4 against task4" $
   solution4 `fulfills` task4
  prop "Testing wrongsolution4 against task4" $
   wrongSolution4 `fulfillsNot` task4

  prop "correct handeling of scoping 1" $
    scopingRight `fulfills` scoping
  prop "correct handeling of scoping 2" $
    scopingWrong `fulfillsNot` scoping

  prop "multi parameter programs" $
    printN `fulfills` printNSpec

  prop "programs build from a spec satisfy that spec" $
    forAll specGen (\s -> buildComputation @IOrep s `fulfills` s)

  -- FIXME: currently broken (timeout), needs to be revised anyway
  -- describe "programs build from a spec satisfy that spec (double negate)" $ do
  --   result <- runIO $ quickCheckWithResult stdArgs{maxSize = 15} $ forAll specGen (\s -> within 5000000 $ buildComputation @IOrep s `fulfillsNot` s)
  --   it "does not fail" $ case result of
  --     Failure{} -> False
  --     Success{} -> False
  --     GaveUp{} -> False
  --     NoExpectedFailure{} -> True
