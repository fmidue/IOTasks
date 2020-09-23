{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.TaskGeneration.Examples where

import Data.Function (on)
import Data.Functor.Contravariant
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

type Program = IOrep ()

behavior :: Specification -> Require Program
behavior s = Require (`fulfills` s)

sampleTrace :: Specification -> Require Trace
sampleTrace s = Require $ \t -> property $ accept s t

haskellProgram :: Specification -> Gen Description
haskellProgram s = haskellCode <$> specProgram s

-- Currently produces not Python code but rather some imperative pseudo-code
pythonProgram :: Specification -> Gen Description
pythonProgram s = pseudoCode <$> specProgram s

specProgram :: Specification -> Gen IRProgram
specProgram p =
  let ps = fst $ runFreshVarM (programVariants =<< programIR' p) emptyVarInfo
  in elements ps

exampleTraces :: Specification -> Int -> Gen [Trace]
exampleTraces s n = do
  ts <- vectorOf (n*3) $ traceGen s
  let ts' = take n $ sortBy (compare `on` length . inputsN) ts --prefer shorter traces
  return $ map (\t -> runProgram (inputsN t) $ buildComputation s) ts'

randomSpecification :: Gen Specification
randomSpecification = oneof [simpleSpec, return example, return example']

-- TODO: use more meaningfull generator
similarSpecifications :: Gen (Specification,Specification)
-- similarSpecifications = oneof [resize 3 simpleSimilar]
similarSpecifications = return (example,example')

trace1 :: TaskTemplate Trace
trace1 = forUnknown randomSpecification $ \s -> do
  prog <- haskellProgram s
  ~[t] <- exampleTraces s 1
  let is = inputs t
  return $
    ( PP.text ("Give the interaction trace of the following program for input(s) " ++ show is ++ "!")
      PP.$$ prog
    ) `solveWith` exactAnswer t

type Input = String

trace2 :: TaskTemplate [Input]
trace2 = forUnknown similarSpecifications $ \(spec1,spec2) -> do
  p1 <- haskellProgram spec1
  p2 <- haskellProgram spec2
  return $
    (PP.text "Give a sequence of input values for which the two programs below behave differently!"
     PP.$$ p1
     PP.$$ PP.text "---"
     PP.$$ p2
    ) `solveWith` triggeringDifference spec1 spec2


triggeringDifference :: Specification -> Specification -> Require [Input]
triggeringDifference s1 s2 = Require $ \is ->
  ((=/=) `on` (runProgram is . buildComputation)) s1 s2

prog1 :: TaskTemplate Program
prog1 = forUnknown randomSpecification $ \s -> do
  ts <- exampleTraces s 5
  return $
    ( PP.text "Write a program capable of these interactions:"
      PP.$$ PP.text "(? represent inputs, ! represent outputs)"
      PP.$$ PP.vcat (map PP.pPrint ts)
    ) `solveWith` producingTraces ts

producingTraces :: [Trace] -> Require Program
producingTraces ts = Require $ \p ->
  property $ all (\t -> runProgram (inputs t) p == t) ts

prog2 :: TaskTemplate Program
prog2 = forFixed example $ \s -> do
  ts <- exampleTraces s 5
  return $
    ( PP.text "Complete the given skeleton into a program capable of these interactions:"
      PP.$$ PP.vcat (map PP.pPrint ts)
      PP.$$ PP.text (unlines
        ["---"
        ,"main :: IO ()"
        ,"main = do"
        ,"  n <- readLn"
        ,"  let loop s m = undefined"
        ,"  loop 0 0"
        ])
    ) `solveWith` producingTraces ts

haskellWithHoles :: Specification -> Gen Description
haskellWithHoles s = haskellWithReadWriteHoles <$> specProgram s

type Code = String

prog3 :: TaskTemplate Code
prog3 = forUnknown randomSpecification $ \s -> do
  prog <- haskellWithHoles s
  return $
    ( PP.text "Complete the following template into a syntactically correct program"
      PP.$$ PP.text "(replace the ??? with calls to readLn and print)"
      PP.$$ prog
    ) `solveWith` compilingProgram

-- TODO: implement, or let submission platform check this.
compilingProgram :: Require Code
compilingProgram = Require $ const $ property True

prog4 :: TaskTemplate (Program, Code)
prog4 = forFixed example $ \spec -> do
  p <- haskellFoldProgram spec
  return $
    ( PP.text "Re-write the given program s.t. it does not contain any accumulation list."
      PP.$$ p
    ) `solveWith` (behavior spec /\ noLists)

noLists :: Require Code
noLists = Require $ \code -> property $ not $ "++" `isInfixOf` code

-- TODO: replace brute force generate and test
haskellFoldProgram :: Specification -> Gen Description
haskellFoldProgram s = do
  p <- specProgram s
    `suchThat` (("++" `isInfixOf`) . PP.render . haskellCode)
  return $ haskellCode p

-- sketch for simplification of prog4 via contramap
prog4' :: TaskTemplate Code
prog4' =  parseCode >$< prog4 where
  parseCode :: Code -> (Program, Code)
  parseCode = undefined

prog5 :: TaskTemplate Program
prog5 = forUnknown randomSpecification $ \s -> do
  prog <- pythonProgram s
  return $
    ( PP.text "Re-implement the following Python program in Haskell:"
      PP.$$ prog
    ) `solveWith` behavior s

desc1 :: TaskTemplate Bool
desc1 = forUnknown similarSpecifications $ \(spec1,spec2) -> do
  sameBehavior <- elements $ False : [ True | hasDifferentPrograms spec1 ] -- TODO: better handle this
  (p1,p2) <-
    if sameBehavior
      then differentPrograms spec1 spec1
      else differentPrograms spec1 spec2
  return $
    ( PP.text "Do the following two programs have the same behavior?"
      PP.$$ p1
      PP.$$ PP.text "---"
      PP.$$ p2
    ) `solveWith` exactAnswer sameBehavior

hasDifferentPrograms :: Specification -> Bool
hasDifferentPrograms s = length variants > 1
  where variants = fst $ runFreshVarM (programVariants =<< programIR' s) emptyVarInfo

differentPrograms  :: Specification -> Specification
                   -> Gen (Description,Description)
differentPrograms s1 s2 = do
  p1 <- haskellProgram s1
  p2 <- haskellProgram s2 `suchThat` (/= p1)
  return (p1,p2)

desc2 :: TaskTemplate [Int]
desc2 = forUnknown similarSpecifications $ \(spec1,spec2) -> do
  p <- haskellProgram spec1
  ts1 <- exampleTraces spec1 2
  ts2 <- exampleTraces spec2 2
  (choices, solution) <- multipleChoicePP 3 ts1 ts2
  return $
    (PP.text "Which of the given trace can the program below produce?"
     PP.$$ p
     PP.$$ choices
    ) `solveWith` exactAnswer solution
