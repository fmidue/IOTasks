{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Task.Examples where

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

import Test.QuickCheck

import Test.IOTasks hiding (Specification)

import Test.IOTasks.Trace (inputs)

import Data.Term.Liftable (litT)
import qualified Data.Term.Liftable.Prelude as T (sum, length, (==))

import Test.IOTasks.Task
import Test.IOTasks.Task.IO

example :: Specification
example =
  readInput "n" nats <>
  tillExit (
    branch ( T.length (getAll @Int "x") T.== getCurrent "n")
     (readInput "x" ints)
     exit
  ) <>
  writeOutput [var 0] [T.sum $ getAll @Int "x"]

example' :: Specification
example' =
  tillExit (
    branch ( T.length (getAll @Int "x") T.== litT 5)
     (readInput "x" ints)
     exit
  ) <>
  writeOutput [var 0] [T.sum $ getAll @Int "x"]

trace1 :: TaskDesign Trace
trace1 = for ((exampleTrace &&&& haskellProgram) `from` randomSpecification) giveInteractionTrace

giveInteractionTrace :: (Trace, Description) -> TaskInstance Trace
giveInteractionTrace (t,prog) =
  TaskInstance
  { question
    = PP.text ("Give the interaction trace of the following program for input(s) " ++ show (inputs t) ++ "!")
    PP.$$ prog

  , given = Nothing
  , requires
    = exactAnswer t
  }

trace2 :: TaskDesign [Input]
trace2 = for ((specification ^&&& haskellProgram) `fromBoth` similarSpecifications) findDiffSequence

findDiffSequence :: ((Specification, Description), (Specification, Description)) -> TaskInstance [Input]
findDiffSequence ((s1,d1), (s2,d2)) =
  TaskInstance
  { question
    = PP.text "Give a sequence of input values for which the two programs below behave differently!"
    PP.$$ d1
    PP.$$ PP.text "---"
    PP.$$ d2

  , given = Nothing
  , requires
    = triggeringDifference s1 s2
  }

prog1 :: TaskDesign HaskellProgram
prog1 = for (exampleTraces 5 `from` randomSpecification) writeProgramForTraces

writeProgramForTraces :: [Trace] -> TaskInstance HaskellProgram
writeProgramForTraces ts =
  TaskInstance
  { question
    = PP.text "Write a program capable of these interactions:"
    PP.$$ PP.text "(? represent inputs, ! represent outputs)"
    PP.$$ PP.vcat (map PP.pPrint ts)

  , given = Nothing
  , requires
    = producingTraces ts
  }

producingTraces :: [Trace] -> Require HaskellProgram
producingTraces ts = requirePure $ \p ->
  all (\t -> runProgram (inputs t) p == t) ts

prog2 :: TaskDesign HaskellCode
prog2 = for (exampleTraces 5 `from` fixed example) completeToMatchInteractions

completeToMatchInteractions :: [Trace] -> TaskInstance HaskellCode
completeToMatchInteractions ts =
  TaskInstance
  { question
    = PP.text "Complete the given skeleton into a program capable of these interactions:"
    PP.$$ PP.vcat (map PP.pPrint ts)

  , given = Just $ fromSourceString $ unlines
    ["main :: IO ()"
    ,"main = do"
    ,"  n <- readLn"
    ,"  let loop s m = undefined"
    ,"  loop 0 0"
    ]

  , requires
    = producingTraces ts `after` compile
  }

prog3 :: TaskDesign HaskellCode
prog3 = for (haskellWithHoles `from` randomSpecification) completeTemplate

completeTemplate :: HaskellCode -> TaskInstance HaskellCode
completeTemplate prog =
  TaskInstance
  { question
    = PP.text "Complete the following template into a syntactically correct program"
    PP.$$ PP.text "(replace the ??? with calls to readLn and print)"
  , given = Just prog

  , requires
    = compilingProgram
  }

prog4 :: TaskDesign HaskellCode
prog4 = for (specificationAnd haskellFoldProgram `from` fixed example) rewriteToNoLists

rewriteToNoLists :: (Specification, Description) -> TaskInstance HaskellCode
rewriteToNoLists (spec, prog) =
  TaskInstance
  { question
    = PP.text "Re-write the given program s.t. it does not contain any accumulation list."
    PP.$$ prog
  , given = Nothing

  , requires
    = (mustSatisfy spec `after` compile) /\ noLists
  }


prog5 :: TaskDesign HaskellProgram
prog5 = for (specificationAnd pythonProgram `from` randomSpecification) implementAsHaskell

implementAsHaskell :: (Specification, Description) -> TaskInstance HaskellProgram
implementAsHaskell (s,prog) =
  TaskInstance
  { question
    = PP.text "Re-implement the following Python program in Haskell:"
    PP.$$ prog
  , given = Nothing

  , requires
    = mustSatisfy s
  }

desc1 :: TaskDesign BinDesc
desc1 = for (equivalenceProblem `from` similarSpecifications) determineEquivalence

determineEquivalence :: (BinDesc, Description, Description) -> TaskInstance BinDesc
determineEquivalence (haveSameBehavior,p1,p2) =
  TaskInstance
  { question
    = PP.text "Do the following two programs have the same behavior?"
    PP.$$ p1
    PP.$$ PP.text "---"
    PP.$$ p2
  , given = Nothing

  , requires
    = exactAnswer haveSameBehavior
  }


desc2 :: TaskDesign [Int]
desc2 = for ((haskellProgram . fst &&&& generateChoices) `from` similarSpecifications) giveProducedTraces

generateChoices :: (Specification, Specification) -> Gen (Description, [Int])
generateChoices (spec1,spec2) = do
  ts1 <- exampleTraces 5 spec1
  ts2 <- exampleTraces 5 spec2
  multipleChoicePP 7 ts1 ts2

giveProducedTraces :: (Description, (Description, [Int])) -> TaskInstance [Int]
giveProducedTraces (p,(choices,solution)) =
  TaskInstance
  { question
    = PP.text "Which of the given trace can the program below produce?"
    PP.$$ p
    PP.$$ choices
  , given = Nothing

  , requires
    = exactAnswer solution
  }
