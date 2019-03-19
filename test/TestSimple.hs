module TestSimple where

import Prelude hiding (getLine, putStrLn, print)

import SpecGen

import Test.IOTest.IOProperty
import Test.IOTest.Translation
import Test.IOTest.Examples.Simple

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, Positive(..))

testSimple :: Spec
testSimple = describe "Testing simple implementation:" $ do
  prop "solution1 matches task1" $
    solution1 `fulfills` task1
  prop "wrongSolution1 does not match against task1" $
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
    buildProgram task1 `fulfills` task1
  prop "program generated from task1' matches task1'" $
    buildProgram task1' `fulfills` task1'
  prop "program generated from task2 matches task2" $
    buildProgram task2 `fulfills` task2
  prop "program generated from task3 matches task3" $
    buildProgram task3 `fulfills` task3
  prop "program generated from task3' matches task3'" $
    buildProgram task3' `fulfills` task3'

  prop "programs build from a simple spec (read and write only) satisfy that spec" $
    forAll specGen (\s -> buildProgram s `fulfills` s)

  prop "correct handeling of scoping 1" $
    scopingRight `fulfills` scoping
  prop "correct handeling of scoping 2" $
    scopingWrong `fulfillsNot` scoping

  prop "multi parameter programs" $
    printN `fulfills` printNSpec
