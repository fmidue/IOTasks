{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.Random.Testing where

import Test.IOTasks.Testing hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args, stdArgs)

import Data.Set as Set hiding (foldr)
import Data.Functor (void)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

import Test.IOTasks.IOrep (IOrep, runProgram)
import Test.IOTasks.Specification
import Test.IOTasks.Trace
import Test.IOTasks.Term
import Test.IOTasks.OutputPattern
import Test.IOTasks.ValueSet
import Test.IOTasks.ValueMap
import Test.IOTasks.Output
import Test.IOTasks.Overflow (OverflowWarning(..))

import Test.QuickCheck (Gen, generate, frequency)

import System.IO (stdout)
import System.Timeout (timeout)
import Control.Monad (when, foldM)

taskCheck :: IOrep () -> Specification -> IO ()
taskCheck = taskCheckWith stdArgs

data Args
  = Args
  { maxPathDepth :: Maybe Int
  , valueSize :: Integer
  , maxSuccess :: Int
  , maxNegative :: Int
  , verbose :: Bool
  , simplifyFeedback :: Bool
  , searchTimeout :: Int -- in milliseconds
  , maxSearchTimeouts :: Int
  }

stdArgs :: Args
stdArgs = Args
  { maxPathDepth = Nothing
  , valueSize = 100
  , maxSuccess = 100
  , maxNegative = 5
  , verbose = True
  , simplifyFeedback = False
  , searchTimeout = 3000 -- 3 sec
  , maxSearchTimeouts = 5
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec  = do
  output <- newOutput stdout
  (outcome, _to) <- foldM (\(o',to) n -> first (o' <>) <$> test output n to) (mempty,0) [0..maxSuccess-1]
  printP output $ (if simplifyFeedback then pPrintOutcomeSimple else pPrintOutcome) outcome
  pure outcome

  where
    test :: Output -> Int -> Int -> IO (Outcome, Int)
    test o n to
      | to > maxSearchTimeouts = pure (Outcome GaveUp NoHints,to)
      | otherwise = do
        input <- generate $ genInput spec maxPathDepth (Size valueSize (fromIntegral $ valueSize `div` 5)) maxNegative
        mOutcome <- timeout (searchTimeout * 1000) $ do
          let o = runTest prog spec input
          seq (isSuccess o) $ pure o -- force outcome
        case mOutcome of
          Just outcome -> do
            when verbose $ putT o (concat ["(",show n," tests)"]) >> oFlush o
            pure (outcome,to)
          Nothing -> do
            when verbose $ putLnP o "input search: timeout"
            test o n (to+1)

genInput :: Specification -> Maybe Int -> Size -> Int -> Gen Inputs
genInput s depth sz maxNeg = do
  t <- genTrace s depth sz maxNeg
  if isTerminating t
    then pure $ inputSequence t
    else genInput s depth sz maxNeg -- repeat sampling until a terminating trace is found

genTrace :: Specification -> Maybe Int -> Size -> Int -> Gen Trace
genTrace spec depth sz maxNeg =
  semM
    (\(e,d,n) x vs mode ->
      (if fromMaybe False ((d >) <$> depth) then pure $ NoRec OutOfInputs
      else do
        frequency $
            (5, valueOf vs sz >>= (\i -> pure $ RecSub (wrapValue i) id (insertValue (wrapValue i) x e,d+1,n)))
          : [(1, valueOf (complement vs) sz >>= (\i -> pure $ RecSame (wrapValue i) id (e,d+1,n+1))) | mode == UntilValid && n < maxNeg]
          ++ [(1, valueOf (complement vs) sz >>= (\i -> pure $ NoRec $ foldr ProgRead Terminate (show i ++ "\n"))) | mode == Abort && n < maxNeg]
    ))
    (pure . \case
      NoRec r -> r
      RecSub i () t' -> do
        foldr ProgRead t' (printValue i ++ "\n")
      RecSame i () t' -> do
        foldr ProgRead t' (printValue i ++ "\n")
      RecBoth{} -> error "genTrace: impossible"
    )
    (\(e,_,_) o ts t' -> ProgWrite o (Set.map (snd . evalPattern e) ts) <$> t')
    (\(e,_,_) c l r -> if snd $ eval c e then l else r)
    (const id)
    (pure Terminate)
    (emptyValueMap $ vars spec,1,0)
    spec

runTest :: IOrep () -> Specification -> Inputs -> Outcome
runTest p spec i =
  let
    (specTrace,warn) = first normalizedTrace $ runSpecification i spec
    progTrace = runProgram i p
    o = case warn of
      OverflowWarning -> OverflowHint 1
      _ -> mempty
  in case specTrace `covers` progTrace of
    MatchSuccessfull -> Outcome (Success 1) o
    failure -> Outcome (Failure i specTrace progTrace failure) o
