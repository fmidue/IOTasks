{-# LANGUAGE RankNTypes #-}
module Test.IOTasks.InputGen where

import Data.Maybe (fromMaybe)
import Data.Holmes as Holmes

import Test.QuickCheck

import Data.Environment.Value
import Data.Term.PTerm
import Data.Environment (Varname)
import Test.IOTasks.Specification
import Test.IOTasks.Constraints

data TestCase = TestCase { testCaseConstraintTree :: ConstraintTree, testCasePath :: Path, testCaseBound :: Int }

instance Show TestCase where
  show (TestCase _ p b) = concat ["TestCase _ ", printConstraints p, " ", show b]

forAllCleverInputs :: Testable prop => Specification (PTerm Varname) -> ([Value] -> prop) -> Property
forAllCleverInputs s prop =
  forAllShrinkBlind
    (genTestCase (constraintTree s))
    shrinkTestCase
    (\tc -> ioProperty $ do
      -- input <- fromMaybe (error $ show tc) <$> solveTestCase Holmes tc -- discard the test case if path is UNSAT
      input <- fromMaybe discard <$> solveTestCase Holmes tc -- discard the test case if path is UNSAT
      pure $ counterexample (show input) $ prop input)
      -- pure $ classify True (show $ head input) $ counterexample (show input) $ prop input)

genTestCase :: ConstraintTree -> Gen TestCase
genTestCase t = sized $ \size -> do
    path <- genPath t (max 1 size)
    b <- getNonNegative <$> arbitrary
    pure $ TestCase t path b

shrinkTestCase :: TestCase -> [TestCase]
shrinkTestCase (TestCase ct p b) =
  -- a. choose a shorter path
  [ TestCase ct p' b | p' <- pathsWithMaxDepth (depth p -1) ct ]
  -- b. restrict the domains of variables
  ++ [ TestCase ct p b' | b' <- shrink b]

genPath :: ConstraintTree -> Int -> Gen Path
genPath t d = do
  -- TODO: add weigths for paths based on number of inputs
  elements $ pathsWithMaxDepth d t

data Solver = Holmes

solveTestCase :: Solver -> TestCase -> IO (Maybe [Value])
solveTestCase Holmes (TestCase _ p b) = solveHolmes p b

-- Holmes
type Bound = Int

solveHolmes :: Path -> Bound -> IO (Maybe [Value])
solveHolmes p b = do
  res <- runHolmes p b
  pure $ (fmap . map) toInput res
  where
    toInput :: Defined Int -> Value
    toInput (Exactly x) = value x show
    toInput Unknown = error "solveHolmes: panic: value unknown"
    toInput Conflict = error "solveHolmes: panic: value conflict"


runHolmes :: Path -> Bound -> IO (Maybe [Defined Int])
runHolmes p b = do
  variables `satisfying` constraints
  -- pure $ fmap f res -- rearange the variables to match the input sequence
  where
    (n,HolmesConstraint constraints) = holmesConstraint p
    variables = Holmes.shuffle $ n `from` [-b..b]
