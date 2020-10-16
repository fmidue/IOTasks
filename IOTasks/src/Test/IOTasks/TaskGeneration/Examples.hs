{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.TaskGeneration.Examples where

import Data.Function (on)
import Data.List (isInfixOf,sortBy)

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.PrettyPrint.HughesPJClass as PP

import Test.QuickCheck

import Test.IOTasks hiding (Specification)
import qualified Test.IOTasks as IOT

import Test.IOTasks.Trace (inputs, inputsN)
import Test.IOTasks.TraceSet (traceGen)
import qualified Test.IOTasks.Trace as Trace (Trace)

import Data.Term.Liftable (litT)
import qualified Data.Term.Liftable.Prelude as T (sum, length, (==))

import Test.IOTasks.TaskGeneration.Task
import Test.IOTasks.CodeGeneration
import Test.IOTasks.SpecGen

type Specification = IOT.Specification SpecTerm
type Trace = Trace.Trace String

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

type HaskellProgram = IOrep ()
newtype HaskellCode = HaskellCode { code :: Description }

fromSourceString :: String -> HaskellCode
fromSourceString = HaskellCode . PP.text

instance Show HaskellCode where
  show (HaskellCode code) = PP.render code

containsFunction :: String -> HaskellCode -> Bool
containsFunction f (HaskellCode code) = f `isInfixOf` PP.render code

instance Matches HaskellCode where
  matches = _

mustSatisfy :: Specification -> Require HaskellProgram
mustSatisfy s = requireProp (`fulfills` s)

compile :: HaskellCode -> IO (Maybe HaskellProgram)
compile = _

sampleTrace :: Specification -> Require Trace
sampleTrace = requirePure . accept

haskellProgram :: Specification -> Gen Description
haskellProgram s = haskellCode <$> specProgram s

-- Currently produces not Python code but rather some imperative pseudo-code
pythonProgram :: Specification -> Gen Description
pythonProgram s = pseudoCode <$> specProgram s

specProgram :: Specification -> Gen IRProgram
specProgram p =
  let ps = fst $ runFreshVarM (programVariants =<< programIR' p) emptyVarInfo
  in elements ps

exampleTraces :: Int -> Specification ->  Gen [Trace]
exampleTraces n s = do
  ts <- vectorOf (n*3) $ traceGen s
  let ts' = take n $ sortBy (compare `on` length . inputsN) ts --prefer shorter traces
  return $ map (\t -> runProgram (inputsN t) $ buildComputation s) ts'

exampleTrace :: Specification -> Gen Trace
exampleTrace = fmap head . exampleTraces 1

randomSpecification :: Gen Specification
randomSpecification = oneof [simpleSpec, return example, return example']

-- TODO: use more meaningfull generator
similarSpecifications :: Gen (Specification,Specification)
-- similarSpecifications = oneof [resize 3 simpleSimilar]
similarSpecifications = return (example,example')

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

type Input = String

trace2 :: TaskDesign [Input]
trace2 = for ((specification ^&&& haskellProgram) `fromBoth` similarSpecifications) findDiffSequence

specification :: Specification -> Specification
specification = id

specificationAnd :: Monad m => (Specification -> m a) -> Specification -> m (Specification,a)
specificationAnd = (id ^&&&)

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

triggeringDifference :: Specification -> Specification -> Require [Input]
triggeringDifference s1 s2 = requireProp $ \is ->
  ((=/=) `on` (runProgram is . buildComputation)) s1 s2

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

haskellWithHoles :: Specification -> Gen HaskellCode
haskellWithHoles s = HaskellCode . haskellWithReadWriteHoles <$> specProgram s

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

-- TODO: implement, or let submission platform check this.
compilingProgram :: Require HaskellCode
compilingProgram = requirePure (const True) `after` compile

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

noLists :: Require HaskellCode
noLists = requirePure $ \code -> not $ containsFunction "++" code

-- TODO: replace brute force generate and test
haskellFoldProgram :: Specification -> Gen Description
haskellFoldProgram s = do
  p <- specProgram s
    `suchThat` (("++" `isInfixOf`) . PP.render . haskellCode)
  return $ haskellCode p

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

data BinDesc = Yes | No deriving (Eq, Ord, Enum, Show)

desc1 :: TaskDesign BinDesc
desc1 = for (equivalenceProblem `from` similarSpecifications) determineEquivalence

equivalenceProblem :: (Specification, Specification) -> Gen (BinDesc, Description, Description)
equivalenceProblem (spec1,spec2) = do
  sameBehavior <- elements $ No : [ Yes | hasDifferentPrograms spec1 ] -- TODO: better handle this
  (p1,p2) <-
    if sameBehavior == Yes
      then differentPrograms spec1 spec1
      else differentPrograms spec1 spec2
  pure (sameBehavior,p1,p2)

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

hasDifferentPrograms :: Specification -> Bool
hasDifferentPrograms s = length variants > 1
  where variants = fst $ runFreshVarM (programVariants =<< programIR' s) emptyVarInfo

differentPrograms  :: Specification -> Specification
                   -> Gen (Description,Description)
differentPrograms s1 s2 = do
  p1 <- haskellProgram s1
  p2 <- haskellProgram s2 `suchThat` (/= p1)
  return (p1,p2)

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
