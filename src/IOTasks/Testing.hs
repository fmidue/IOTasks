{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.Testing where

import IOTasks.IOrep (IOrep, Line, runProgram)
import IOTasks.Specification
import IOTasks.Constraints
import IOTasks.Trace
import IOTasks.Z3

import Control.Monad (when, forM, replicateM)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Functor (void)

import Text.PrettyPrint hiding ((<>))

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
  -- let ps = sortOn pathDepth $ paths maxPathDepth $ constraintTree maxNegative spec
  let ps = paths maxPathDepth $ constraintTree maxNegative spec
  (out,satPaths,nInputs,timeouts) <- testPaths ps (0,0,0) Nothing
  --
  when verbose $ do
    putStrLn $ unwords
      ["generated", show nInputs,"inputs covering", show satPaths,"satisfiable paths ("++ show (length ps),"paths with max. depth",show maxPathDepth,"in total)"]
    when (timeouts > 0) $
      putStrLn $ unwords ["---",show timeouts, "paths timed out"]
  --
  print $ (if simplifyFeedback then pPrintOutcomeSimple else pPrintOutcome) out
  pure out

  where
    testPaths :: [Path] -> (Int,Int,Int) -> Maybe Outcome -> IO (Outcome,Int,Int,Int)
    testPaths [] (m,n,t) (Just failure) = pure (failure,m,n,t)
    testPaths [] (m,n,t) Nothing = pure (Success n,m,n,t)
    testPaths _ (m,n,t) (Just failure) | t > maxTimeouts = pure (failure,m,n,t)
    testPaths _ (m,n,t) Nothing | t > maxTimeouts = pure (GaveUp,m,n,t)
    testPaths (p:ps) (m,n,t) mFailure = do
      sat <- isSatPath solverTimeout p
      if sat -- does not account for timeouts yet
        then do
          (out,k) <- testPath p 0 n
          case out of
            PathSuccess -> testPaths ps (m+1,n+k,t) mFailure
            PathFailure i et at r -> testPaths (filter ((< pathDepth p) . pathDepth) ps) (m+1,n+k,t) (Just $ Failure i et at r)
            PathTimeout
              | k > 0 -> testPaths ps (m+1,n+k,t) mFailure
              | otherwise -> testPaths ps (m,n,t+1) mFailure
        else testPaths ps (m,n,t) mFailure
    testPath :: Path -> Int -> Int -> IO (PathOutcome,Int)
    testPath _ n _ | n >= maxSuccessPerPath = pure (PathSuccess,n)
    testPath p n nOtherTests = do
      mNextInput <- fmap @Maybe (map show) <$> findPathInput solverTimeout p valueSize
      case mNextInput of
        Nothing -> pure (PathTimeout,n) -- should (only?) be the case if solving times out
        Just nextInput  -> do
          when verbose $ putStr $ concat ["(",show (n+nOtherTests)," tests)\r"]
          let
            specTrace = runSpecification nextInput spec
            progTrace = runProgram nextInput prog
          case specTrace `covers` progTrace of
            MatchSuccessfull -> testPath p (n+1) nOtherTests
            failure -> pure (PathFailure nextInput specTrace progTrace failure,n+1)

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
      specTrace = runSpecification i spec
      progTrace = runProgram i prog
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) is prog spec
      failure -> Failure i specTrace progTrace failure

generateStaticTestSuite :: Args -> Specification -> IO [Inputs]
generateStaticTestSuite Args{..} spec =
  let ps = sortOn pathDepth $ paths maxPathDepth $ constraintTree maxNegative spec
  in map (map show) . concat <$> forM ps (\p -> catMaybes <$> replicateM maxSuccessPerPath (findPathInput solverTimeout p valueSize))
