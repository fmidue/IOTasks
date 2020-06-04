{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module TestExamples where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTasks
import Test.IOTasks.TraceSet
import Test.IOTasks.SpecGen
import Test.IOTasks.Trace

import Test.IOTasks.Examples.SampleTasks

import Data.Environment (Environment)
import Data.Term.Class (SemTerm, VarListTerm)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Control.DeepSeq
import GHC.Generics

buildIOrepComputation :: (VarListTerm t Varname, SemTerm t (Environment Varname)) => Specification t -> IOrep ()
buildIOrepComputation = buildComputation

testExamples :: Spec
testExamples = describe "Testing Test.IOTasks.Examples.Examples:" $ do
  -- Example 1
  prop "solution1 matches ex1" $
    solution1 `fulfills` ex1
  prop "solution1 matches ex1Combinators" $
    solution1 `fulfills` ex1Combinators

  prop "solution1Pat matches ex1Pattern" $
    solution1Pat `fulfills` ex1Pattern
  prop "solution1PatCombinators matches ex1Pattern" $
    solution1Pat `fulfills` ex1PatternCombinators

  prop "wrongSolutionPat1 does not match ex1Pattern" $
    expectFailure $ wrongSolutionPat1 `fulfills` ex1Pattern
  prop "wrongSolutionPat1 does not match ex1PatternCombinators" $
    expectFailure $ wrongSolutionPat1 `fulfills` ex1PatternCombinators

  prop "solution1Pat' matches ex1Pattern" $
    solution1Pat' `fulfills` ex1Pattern

  -- Example 2
  prop "solution2 matches ex2" $
   solution2 `fulfills` ex2

  -- Example 3
  prop "solution3 matches ex3" $
    solution3 `fulfills` ex3
  prop "solution3 matches ex3Combinators" $
    solution3 `fulfills` ex3Combinators

  -- Example 4:
  prop "solution4 matches ex4" $
    solution4 `fulfills` ex4
  prop "wrongSolution4 does not match ex4" $
    expectFailure $ wrongSolution4 `fulfills` ex4

  -- Example 5:
  prop "multi parameter programs" $
    printN `fulfills` printNSpec

  -- Generation of programs
  prop "program generated from ex1 matches ex1" $
    buildIOrepComputation ex1 `fulfills` ex1
  prop "program generated from ex1Combinators matches ex1Combinators" $
    buildIOrepComputation ex1Combinators `fulfills` ex1Combinators
  prop "program generated from ex1Pattern matches ex1Pattern" $
    buildIOrepComputation ex1Pattern `fulfills` ex1Pattern
  prop "program generated from ex1PatternCombinators matches ex1PatternCombinators" $
    buildIOrepComputation ex1PatternCombinators `fulfills` ex1PatternCombinators
  prop "program generated from ex2 matches ex2" $
    buildIOrepComputation ex2 `fulfills` ex2
  prop "program generated from ex3 matches ex3" $
    buildIOrepComputation ex3 `fulfills` ex3
  prop "program generated from ex3Combinators matches ex3Combinators" $
    buildIOrepComputation ex3Combinators `fulfills` ex3Combinators
  prop "program generated from ex4 matches ex4" $
    buildIOrepComputation ex4 `fulfills` ex4

  -- aditional general properties
  prop "programs built from a spec satisfy that spec" $
    forAll specGen (\s -> buildIOrepComputation s `fulfills` s)

  prop "programs built from a spec dont go wrong on inputs generated from the same spec" $
    forAll specGen (\s ->
      forAll (traceGen s) (\t ->
        let is = inputsN t
        in runProgram is (buildComputation s) `deepseq` True
    ))

  prop "relate traceGen and accept" $
    forAll specGen (\s -> forAll (traceGen s) (\t' -> forAll (sampleNTrace t') (accept s)))

  -- currently not working: triggers "Prelude.read: no parse" in buildComputation
  -- prop "inputs are never optional for a fixed input prefix" $
  --   forAll specGen (\s ->
  --     forAll (traceGen s) (\t ->
  --       let is = inputsN t
  --       in not (null is) ==> fulfillsNotFor @(IOrep ()) (init is) (buildComputation s) s))

  prop "tillExit s === tillExit (s <> tillExit s <> exit) " $
    forAll loopBodyGen $ \(s,i) -> testEquiv
      (i <> tillExit s)
      (i <> tillExit (s <> tillExit s <> exit))

  prop "s1 <> (s2 <> s3) === (s1 <> s2) <> s3" $
    forAll ((,,) <$> specGen <*> specGen <*> specGen) $
      \(s1,s2,s3) -> testEquiv
        (s1 <> (s2 <> s3))
        ((s1 <> s2) <> s3)

  prop "tillExit (s1 <> exit) === tillExit (s1 <> exit <> s2)" $
    forAll ((,) <$> specGen <*> specGen) $
      \(s1,s2) -> testEquiv
        (tillExit (s1 <> exit))
        (tillExit (s1 <> exit <> s2))

testEquiv :: (SemTerm t (Environment Varname), VarListTerm t Varname) => Specification t -> Specification t -> Property
testEquiv s1 s2 = p1 .&&. p2 where
  p1 = s1 `testAgainst` s2
  p2 = s2 `testAgainst` s1
  testAgainst x y =
    forAll (traceGen x) (\t ->
      forAll (sampleNTrace t) (accept y))

deriving instance Generic (Trace a)
deriving instance NFData a => NFData (Trace a)
