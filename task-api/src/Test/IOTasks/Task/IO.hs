module Test.IOTasks.Task.IO where

import Data.Function (on)
import Data.List (isInfixOf,sortBy)

import qualified Text.PrettyPrint.HughesPJ as PP

import Test.QuickCheck

import Test.IOTasks hiding (Specification)
import qualified Test.IOTasks as IOT

import Test.IOTasks.Trace (inputsN)
import Test.IOTasks.TraceSet (traceGen)
import qualified Test.IOTasks.Trace as Trace (Trace)

import Test.IOTasks.Task
import Test.IOTasks.CodeGeneration
import Test.IOTasks.SpecGen

type Specification = IOT.Specification SpecTerm
type Trace = Trace.Trace String

type HaskellProgram = IOrep ()
newtype HaskellCode = HaskellCode { code :: Description }

type Input = String

fromSourceString :: String -> HaskellCode
fromSourceString = HaskellCode . PP.text

instance Show HaskellCode where
  show = PP.render . code

containsFunction :: String -> HaskellCode -> Bool
containsFunction f = (f `isInfixOf`) . PP.render . code

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
randomSpecification = simpleSpec
-- randomSpecification = oneof [simpleSpec, return example, return example']

-- TODO: use more meaningfull generator
similarSpecifications :: Gen (Specification,Specification)
similarSpecifications = oneof [resize 3 simpleSimilar]
-- similarSpecifications = return (example,example')

specification :: Specification -> Specification
specification = id

specificationAnd :: Monad m => (Specification -> m a) -> Specification -> m (Specification,a)
specificationAnd = (id ^&&&)

triggeringDifference :: Specification -> Specification -> Require [Input]
triggeringDifference s1 s2 = requireProp $ \is ->
  ((=/=) `on` (runProgram is . buildComputation)) s1 s2

haskellWithHoles :: Specification -> Gen HaskellCode
haskellWithHoles s = HaskellCode . haskellWithReadWriteHoles <$> specProgram s

-- TODO: implement, or let submission platform check this.
compilingProgram :: Require HaskellCode
compilingProgram = requirePure (const True) `after` compile

noLists :: Require HaskellCode
noLists = requirePure $ not . containsFunction "++"

data BinDesc = Yes | No deriving (Eq, Ord, Enum, Show)

equivalenceProblem :: (Specification, Specification) -> Gen (BinDesc, Description, Description)
equivalenceProblem (spec1,spec2) = do
  sameBehavior <- elements $ No : [ Yes | hasDifferentPrograms spec1 ] -- TODO: better handle this
  (p1,p2) <-
    if sameBehavior == Yes
      then differentPrograms spec1 spec1
      else differentPrograms spec1 spec2
  pure (sameBehavior,p1,p2)

hasDifferentPrograms :: Specification -> Bool
hasDifferentPrograms s = length variants > 1
  where variants = fst $ runFreshVarM (programVariants =<< programIR' s) emptyVarInfo

differentPrograms  :: Specification -> Specification
                   -> Gen (Description,Description)
differentPrograms s1 s2 = do
  p1 <- haskellProgram s1
  p2 <- haskellProgram s2 `suchThat` (/= p1)
  return (p1,p2)

-- TODO: replace brute force generate and test
haskellFoldProgram :: Specification -> Gen Description
haskellFoldProgram s = do
  p <- specProgram s
    `suchThat` (("++" `isInfixOf`) . PP.render . haskellCode)
  return $ haskellCode p
