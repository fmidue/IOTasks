{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module IOTasks.Testing where

import IOTasks.IOrep (IOrep, Line, runProgram)
import IOTasks.Specification
import IOTasks.Constraints
import IOTasks.Trace
import IOTasks.Z3
import IOTasks.Overflow

import Control.Concurrent.STM
import Control.Monad (when, forM, replicateM)

import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Functor (void)

import Text.PrettyPrint hiding ((<>))
import Control.Concurrent (forkIO, killThread)
import Control.Exception (finally)
import System.IO

taskCheck :: IOrep () -> Specification -> IO ()
taskCheck = taskCheckWith stdArgs

data Args
  = Args
  { maxPathDepth :: Int -- maximum number of inputs to consider
  , valueSize :: Integer -- size of randomly generated input candidates (the solver might find bigger solutions)
  , solverTimeout :: Int -- solver timeout in miliseconds
  , maxTimeouts :: Int -- maximum number of solver timeouts before giving up
  , maxSuccessPerPath :: Int -- number of tests generated per path
  , maxNegative :: Int -- maximum number of negative inputs per path
  , verbose :: Bool -- print extra information
  , simplifyFeedback :: Bool -- cleanup feedback for educational use
  }

stdArgs :: Args
stdArgs = Args
  { maxPathDepth = 25
  , valueSize = 100
  , solverTimeout = 1000
  , maxTimeouts = 3
  , maxSuccessPerPath = 5
  , maxNegative = 5
  , verbose = True
  , simplifyFeedback = False
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec = do
  q <- atomically newTQueue
  nVar <- newTVarIO maxPathDepth
  thrdID <- forkIO $ satPaths nVar solverTimeout (constraintTree maxNegative spec) q
  (out,satPaths,nInputs,timeouts) <- testPaths nVar q (0,0,0) Nothing `finally` killThread thrdID

  --
  when verbose $ do
    putStrLn $ unwords
      ["generated", show nInputs,"inputs covering", show satPaths, "satisfiable paths"]
    when (timeouts > 0) $
      putStrLn $ unwords ["---",show timeouts, "paths timed out"]
  --
  print $ (if simplifyFeedback then pPrintOutcomeSimple else pPrintOutcome) out
  pure out

  where
    testPaths :: TVar Int -> TQueue (Maybe Path) -> (Int,Int,Int) -> Maybe Outcome -> IO (Outcome,Int,Int,Int)
    testPaths _ _ (m,n,t) (Just failure) | t > maxTimeouts = pure (failure,m,n,t)
    testPaths _ _ (m,n,t) Nothing | t > maxTimeouts = pure (GaveUp,m,n,t)
    testPaths nVar q (m,n,t) mFailure = do
      p <- atomically $ readTQueue q
      currentMaxDepth <- readTVarIO nVar
      case p of
        Nothing -> case mFailure of -- no more paths
           (Just failure) -> pure (failure,m,n,t)
           Nothing -> pure (Success n,m,n,t)
        Just p
          | currentMaxDepth < pathDepth p -> testPaths nVar q (m,n,t) mFailure
          | otherwise -> do
          res <- isSatPath solverTimeout p
          if res == SAT -- does not account for timeouts yet
            then do
              (out,k) <- testPath p 0 n
              case out of
                PathSuccess -> do
                  testPaths nVar q (m+1,n+k,t) mFailure
                PathFailure i et at r ->
                  if maybe True (\case {Failure j _ _ _ -> length i < length j; _ -> error "impossible"}) mFailure -- there might be paths in the queue that are longer than a found counterexample
                    then do
                      atomically $ writeTVar nVar (length i - 1)
                      testPaths nVar q (m+1,n+k,t) (Just $ Failure i et at r)
                    else
                      testPaths nVar q (m+1,n+k,t) mFailure
                PathTimeout
                  | k > 0 -> testPaths nVar q (m+1,n+k,t) mFailure
                  | otherwise -> testPaths nVar q (m,n,t+1) mFailure
            else testPaths nVar q (m,n,t) mFailure
    testPath :: Path -> Int -> Int -> IO (PathOutcome,Int)
    testPath _ n _ | n >= maxSuccessPerPath = pure (PathSuccess,n)
    testPath p n nOtherTests = do
      mNextInput <- fmap @Maybe (map show) <$> findPathInput solverTimeout p valueSize
      case mNextInput of
        Nothing -> pure (PathTimeout,n) -- should (only?) be the case if solving times out
        Just nextInput  -> do
          when verbose (putStr (concat ["(",show (n+nOtherTests)," tests)\r"]) >> hFlush stdout)
          let
            (specTrace,warn) = runSpecification nextInput spec
            progTrace = runProgram nextInput prog
          when (warn == Overflow) $ putStrLn "Overflow of Int range detected."
          case specTrace `covers` progTrace of
            MatchSuccessfull -> testPath p (n+1) nOtherTests
            failure -> do
              when verbose $ putStrLn $ unwords ["found counterexample of length",show $ length nextInput]
              pure (PathFailure nextInput specTrace progTrace failure,n+1)

type Inputs = [Line]
type ExpectedRun = Trace
type ActualRun = Trace

data Outcome = Success Int | Failure Inputs ExpectedRun ActualRun MatchResult | GaveUp
  deriving (Eq, Show)

isSuccess :: Outcome -> Bool
isSuccess Success{} = True
isSuccess _ = False

data PathOutcome = PathSuccess | PathTimeout | PathFailure Inputs ExpectedRun ActualRun MatchResult
  deriving (Eq,Show)

pPrintOutcome :: Outcome -> Doc
pPrintOutcome = pPrintOutcome' False

pPrintOutcomeSimple :: Outcome -> Doc
pPrintOutcomeSimple = pPrintOutcome' True

pPrintOutcome' :: Bool -> Outcome -> Doc
pPrintOutcome' _ (Success n) = text $ unwords ["+++ OK, passed",show n,"tests."]
pPrintOutcome' _ GaveUp = text "*** Gave up!"
pPrintOutcome' simple (Failure is et at r) = vcat
  [ text "*** Failure"
  , text ("Input sequence "++ pPrintInputs is)
  , text ("Expected run: " ++ printTrace et)
  , text ("Actual run: " ++ printTrace at)
  , text "Error:"
  , nest 2 (printResult r)
  ]
  where
    (printResult, printTrace)
      | simple = (pPrintMatchResultSimple,pPrintTraceSimple)
      | otherwise = (pPrintMatchResult,pPrintTrace)

pPrintInputs :: Inputs -> String
pPrintInputs = unwords . map ('?':)

-- static test suite generation
taskCheckOn :: [Inputs] -> IOrep () -> Specification -> Outcome
taskCheckOn = go 0 where
  go n [] _ _ = Success n
  go n (i:is) prog spec =
    let
      (specTrace,w) = runSpecification i spec
      progTrace = runProgram i prog
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) is prog spec
      failure -> Failure i specTrace progTrace failure

generateStaticTestSuite :: Args -> Specification -> IO [Inputs]
generateStaticTestSuite Args{..} spec =
  let ps = sortOn pathDepth $ paths maxPathDepth $ constraintTree maxNegative spec
  in map (map show) . concat <$> forM ps (\p -> catMaybes <$> replicateM maxSuccessPerPath (findPathInput solverTimeout p valueSize))
