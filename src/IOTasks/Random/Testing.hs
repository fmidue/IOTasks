{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module IOTasks.Random.Testing where

import IOTasks.Testing hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args, stdArgs)

import Data.Map as Map hiding (foldr)
import Data.Set as Set hiding (foldr)
import Data.Functor (void)

import IOTasks.IOrep (IOrep, runProgram)
import IOTasks.Specification
import IOTasks.Trace
import IOTasks.Term
import IOTasks.OutputPattern
import IOTasks.ValueSet

import Test.QuickCheck (Gen, vectorOf, generate, frequency)

taskCheck :: IOrep () -> Specification -> IO ()
taskCheck = taskCheckWith stdArgs

data Args
  = Args
  { maxPathDepth :: Int
  , valueSize :: Integer
  , maxSuccess :: Int
  , maxNegative :: Int
  , verbose :: Bool
  , simplifyFeedback :: Bool
  }

stdArgs :: Args
stdArgs = Args
  { maxPathDepth = 25
  , valueSize = 100
  , maxSuccess = 100
  , maxNegative = 5
  , verbose = True
  , simplifyFeedback = False
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec  = do
  is <- generate $ vectorOf maxSuccess $ genInput spec maxPathDepth valueSize maxNegative
  let outcome = runTests prog spec is
  print $ (if simplifyFeedback then pPrintOutcomeSimple else pPrintOutcome) outcome
  pure outcome

genInput :: Specification -> Int -> Integer -> Int -> Gen Inputs
genInput s depth bound maxNeg = do
  t <- genTrace s depth bound maxNeg
  if isTerminating t
    then pure $ inputSequence t
    else genInput s depth bound maxNeg -- repeat sampling until a terminating trace is found

genTrace :: Specification -> Int -> Integer -> Int -> Gen Trace
genTrace spec depth bound maxNeg =
  semM
    (\(e,d,n) x vs mode ->
      (if d > depth then pure NoRec
      else do
        frequency $
            (5, valueOf vs bound >>= (\i -> pure $ RecSub i (Map.update (\xs -> Just $ (i,d):xs) x e,d+1,n)))
          : [(1, valueOf (complement vs) bound >>= (\i -> pure $ RecSame i (e,d+1,n+1))) | mode == UntilValid && n < maxNeg]
    ))
    (pure . \case
      NoRec -> OutOfInputs
      RecSub i t' -> do
        foldr ProgRead t' (show i ++ "\n")
      RecSame i t' -> do
        foldr ProgRead t' (show i ++ "\n")
      RecBoth{} -> error "genTrace: impossible"
    )
    (\(e,_,_) o ts t' -> ProgWrite o (Set.map (evalPattern e) ts) <$> t')
    (\(e,_,_) c l r -> if eval c e then l else r)
    (pure Terminate)
    (Map.fromList ((,[]) <$> vars spec),1,0)
    spec

runTests :: IOrep () -> Specification -> [Inputs] -> Outcome
runTests = go 0 where
  go n _ _ [] = Success n
  go n prog spec (i:is) =
    let
      specTrace = runSpecification i spec
      progTrace = runProgram i prog
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) prog spec is
      failure -> Failure i specTrace progTrace failure
