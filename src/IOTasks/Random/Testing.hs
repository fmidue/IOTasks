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
import IOTasks.ValueMap

import Test.QuickCheck (Gen, vectorOf, generate, frequency)
import IOTasks.Overflow (OverflowWarning(..))

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
  is <- generate $ vectorOf maxSuccess $ genInput spec maxPathDepth (Size valueSize (fromIntegral $ valueSize `div` 5)) maxNegative
  let outcome = runTests prog spec is
  print $ (if simplifyFeedback then pPrintOutcomeSimple else pPrintOutcome) outcome
  pure outcome

genInput :: Specification -> Int -> Size -> Int -> Gen Inputs
genInput s depth sz maxNeg = do
  t <- genTrace s depth sz maxNeg
  if isTerminating t
    then pure $ inputSequence t
    else genInput s depth sz maxNeg -- repeat sampling until a terminating trace is found

genTrace :: Specification -> Int -> Size -> Int -> Gen Trace
genTrace spec depth sz maxNeg =
  semM
    (\(e,d,n) x vs mode ->
      (if d > depth then pure $ NoRec OutOfInputs
      else do
        frequency $
            (5, valueOf vs sz >>= (\i -> pure $ RecSub (wrapValue i) (insertValue (wrapValue i,d) x e,d+1,n)))
          : [(1, valueOf (complement vs) sz >>= (\i -> pure $ RecSame (wrapValue i) (e,d+1,n+1))) | mode == UntilValid && n < maxNeg]
          ++ [(1, valueOf (complement vs) sz >>= (\i -> pure $ NoRec $ foldr ProgRead Terminate (show i ++ "\n"))) | mode == Abort && n < maxNeg]
    ))
    (pure . \case
      NoRec r -> r
      RecSub i t' -> do
        foldr ProgRead t' (printValue i ++ "\n")
      RecSame i t' -> do
        foldr ProgRead t' (printValue i ++ "\n")
      RecBoth{} -> error "genTrace: impossible"
    )
    (\(e,_,_) o ts t' -> ProgWrite o (Set.map (snd . evalPattern e) ts) <$> t')
    (\(e,_,_) c l r -> if snd $ eval c e then l else r)
    (pure Terminate)
    (Map.fromList ((,NoEntry) <$> vars spec),1,0)
    spec

runTests :: IOrep () -> Specification -> [Inputs] -> Outcome
runTests p s i = uncurry Outcome $ go 0 0 p s i where
  go n o _ _ [] = (Success n, overflowHint o)
  go n o prog spec (i:is) =
    let
      (specTrace,warn) = runSpecification i spec
      progTrace = runProgram i prog
      o' = if warn == OverflowWarning then o+1 else o
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) o' prog spec is
      failure -> (Failure i specTrace progTrace failure, overflowHint o')

  overflowHint 0 = NoHints
  overflowHint n = OverflowHint n
