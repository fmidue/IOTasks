{-# LANGUAGE FlexibleContexts #-}
module TestPTermExamples where

import Test.IOTasks

import Test.IOTasks.Examples.PTermExamples
import Test.IOTasks.Examples.SampleTasks
  (solution1,solution2,solution3
  ,solution1Pat, solution1Pat'
  ,wrongSolutionPat1
  )

import Data.Environment (Environment)
import Data.Term.Class (SemTerm, VarListTerm)

import Test.Hspec
import Test.Hspec.QuickCheck (prop, modifyMaxSize)
import Test.QuickCheck

buildIOrepComputation :: (VarListTerm t Varname, SemTerm t (Environment Varname)) => Specification t -> IOrep ()
buildIOrepComputation = buildComputation

testPTermExamples :: Spec
testPTermExamples = describe "Testing Test.IOTasks.Examples.PTermExamples:" $ do
  -- Example 1
  testPairSuccess (solution1,"solution1") (ex1P,"ex1P")
  testPairSuccess (solution1Pat,"solution1Pat") (ex1PPattern,"ex1PPattern")
  testPairFailure (wrongSolutionPat1 ,"wrongSolutionPat1") (ex1PPattern,"ex1PPattern")

  -- Example 2
  modifyMaxSize (const 30) $ testPairSuccess (solution2,"solution2") (ex2P,"ex2P")

  -- Example 3
  testPairSuccess (solution3,"solution3") (ex3P,"ex3P")

  -- Generation of programs
  prop "program generated from ex1P matches ex1P" $
    buildIOrepComputation ex1P `fulfills` ex1P
  prop "program generated from ex1Pattern matches ex1Pattern" $
    buildIOrepComputation ex1PPattern `fulfills` ex1PPattern
  prop "program generated from ex2P matches ex2P" $
    buildIOrepComputation ex2P `fulfills` ex2P
  prop "program generated from ex3 matches ex3" $
    buildIOrepComputation ex3P `fulfills` ex3P

testPairSuccess (prog,pname) (spec,sname) = do
  prop (pname ++ " matches " ++ sname) $
    prog `fulfills` spec
  prop (pname ++ " matches " ++ sname ++ " (clever)") $
    prog `fulfillsClever` spec

testPairFailure (prog,pname) (spec,sname) = do
  prop (pname ++ " does not match " ++ sname) $
    expectFailure $ prog `fulfills` spec
  prop (pname ++ " matches " ++ sname ++ " (clever)") $
    expectFailure $ prog `fulfillsClever` spec
