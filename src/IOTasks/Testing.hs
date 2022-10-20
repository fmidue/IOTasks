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
  , checkOverflows :: Bool -- check that intermediate results do not cause Int overflows, including (parts of OutputTerms when possible) (best-effort, no gurantees on completness)
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
  , checkOverflows = False
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

type SatPaths = Int
type NumberOfInputs = Int
type Timeouts = Int
type Overflows = Int
type TestsRun = Int

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec = do
  q <- atomically newTQueue
  nVar <- newTVarIO maxPathDepth
  thrdID <- forkIO $ satPaths nVar solverTimeout (constraintTree maxNegative spec) checkOverflows q
  (coreOut,satPaths,nInputs,timeouts,overflows) <- testPaths nVar q (0,0,0,0) Nothing `finally` killThread thrdID

  let out = Outcome coreOut (if overflows == 0 then NoHints else OverflowHint overflows)
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
    testPaths :: TVar Int -> TQueue (Maybe Path) -> (SatPaths,NumberOfInputs,Timeouts,Overflows) -> Maybe CoreOutcome -> IO (CoreOutcome,SatPaths,NumberOfInputs,Timeouts,Overflows)
    testPaths _ _ (m,n,t,o) (Just failure) | t > maxTimeouts = pure (failure,m,n,t,o)
    testPaths _ _ (m,n,t,o) Nothing | t > maxTimeouts = pure (GaveUp,m,n,t,o)
    testPaths nVar q (m,n,t,o) mFailure = do
      p <- atomically $ readTQueue q
      currentMaxDepth <- readTVarIO nVar
      case p of
        Nothing -> case mFailure of -- no more paths
           (Just failure) -> pure (failure,m,n,t,o)
           Nothing -> pure (Success n,m,n,t,o)
        Just p
          | currentMaxDepth < pathDepth p -> testPaths nVar q (m,n,t,o) mFailure
          | otherwise -> do
          res <- isSatPath solverTimeout p checkOverflows
          if res == SAT -- does not account for timeouts yet
            then do
              (out,k,o') <- testPath p 0 n 0
              case out of
                PathSuccess -> do
                  testPaths nVar q (m+1,n+k,t,o+o') mFailure
                PathFailure i et at r ->
                  if maybe True (\case {Failure j _ _ _ -> length i < length j; _ -> error "impossible"}) mFailure -- there might be paths in the queue that are longer than a found counterexample
                    then do
                      atomically $ writeTVar nVar (length i - 1)
                      testPaths nVar q (m+1,n+k,t,o+o') (Just $ Failure i et at r)
                    else
                      testPaths nVar q (m+1,n+k,t,o+o') mFailure
                PathTimeout
                  | k > 0 -> testPaths nVar q (m+1,n+k,t,o+o') mFailure
                  | otherwise -> testPaths nVar q (m,n,t+1,o+o') mFailure
            else testPaths nVar q (m,n,t,o) mFailure
    testPath :: Path -> TestsRun -> NumberOfInputs -> Overflows -> IO (PathOutcome,TestsRun,Overflows)
    testPath _ n _ o | n >= maxSuccessPerPath = pure (PathSuccess,n,o)
    testPath p n nOtherTests o = do
      mNextInput <- fmap @Maybe (map show) <$> findPathInput solverTimeout p valueSize checkOverflows
      case mNextInput of
        Nothing -> pure (PathTimeout,n,o) -- should (only?) be the case if solving times out
        Just nextInput  -> do
          when verbose (putStr (concat ["(",show (n+nOtherTests)," tests)\r"]) >> hFlush stdout)
          let
            (specTrace,warn) = runSpecification nextInput spec
            progTrace = runProgram nextInput prog
            o' = if warn == OverflowWarning then o+1 else o
          when (verbose && warn == OverflowWarning) $ putStrLn "Overflow of Int range detected."
          case specTrace `covers` progTrace of
            MatchSuccessfull -> testPath p (n+1) nOtherTests o'
            failure -> do
              when verbose $ putStrLn $ unwords ["found counterexample of length",show $ length nextInput]
              pure (PathFailure nextInput specTrace progTrace failure,n+1,o')

type Inputs = [Line]
type ExpectedRun = Trace
type ActualRun = Trace

data Outcome = Outcome CoreOutcome OutcomeHints
  deriving (Eq, Show)

data CoreOutcome = Success Int | Failure Inputs ExpectedRun ActualRun MatchResult | GaveUp
  deriving (Eq, Show)

data OutcomeHints = NoHints | OverflowHint Int
  deriving (Eq, Show)

isSuccess :: Outcome -> Bool
isSuccess (Outcome Success{} _) = True
isSuccess _ = False

overflowWarnings :: Outcome -> Int
overflowWarnings (Outcome _ NoHints) = 0
overflowWarnings (Outcome _ (OverflowHint n)) = n

data PathOutcome = PathSuccess | PathTimeout | PathFailure Inputs ExpectedRun ActualRun MatchResult
  deriving (Eq,Show)

pPrintOutcome :: Outcome -> Doc
pPrintOutcome (Outcome core hints) = pPrintOutcomeHints hints $+$ pPrintCoreOutcome False core

pPrintOutcomeSimple :: Outcome -> Doc
pPrintOutcomeSimple (Outcome core hints) = pPrintOutcomeHints hints $+$ pPrintCoreOutcome True core

pPrintCoreOutcome :: Bool -> CoreOutcome -> Doc
pPrintCoreOutcome _ (Success n) = text $ unwords ["+++ OK, passed",show n,"tests."]
pPrintCoreOutcome _ GaveUp = text "*** Gave up!"
pPrintCoreOutcome simple (Failure is et at r) = vcat
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

pPrintOutcomeHints :: OutcomeHints -> Doc
pPrintOutcomeHints NoHints = mempty
pPrintOutcomeHints (OverflowHint n) = text $ unwords [show n,"overflows of Int range where detected during testing"]

-- static test suite generation
taskCheckOn :: [Inputs] -> IOrep () -> Specification -> Outcome
taskCheckOn i p s = uncurry Outcome (go 0 0 i p s) where
  go n o [] _ _ = (Success n, overflowHint o)
  go n o (i:is) prog spec =
    let
      (specTrace,warn) = runSpecification i spec
      progTrace = runProgram i prog
      o' = if warn == OverflowWarning then o+1 else o
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) o' is prog spec
      failure -> (Failure i specTrace progTrace failure, overflowHint o')

  overflowHint 0 = NoHints
  overflowHint n = OverflowHint n

generateStaticTestSuite :: Args -> Specification -> IO [Inputs]
generateStaticTestSuite Args{..} spec =
  let ps = sortOn pathDepth $ paths maxPathDepth $ constraintTree maxNegative spec
  in map (map show) . concat <$> forM ps (\p -> catMaybes <$> replicateM maxSuccessPerPath (findPathInput solverTimeout p valueSize checkOverflows))
