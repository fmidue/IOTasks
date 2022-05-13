{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Testing where

import IOrep (IOrep, Line, runProgram)
import Specification
import Constraints
import Trace
import Z3

import Control.Monad (when, forM, replicateM)
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Functor (void)

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
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec = do
  let ps = sortOn pathDepth $ paths maxPathDepth $ constraintTree maxNegative spec
  (out,satPaths,nInputs,timeouts) <- testPaths ps (0,0,0)
  --
  when verbose $ do
    putStrLn $ unwords
      ["generated", show nInputs,"inputs covering", show satPaths,"satisfiable paths ("++ show (length ps),"paths with max. depth",show maxPathDepth,"in total)"]
    when (timeouts > 0) $
      putStrLn $ unwords ["---",show timeouts, "paths timed out"]
  --
  putStrLn $ pPrintOutcome out
  pure out

  where
    testPaths :: [Path] -> (Int,Int,Int) -> IO (Outcome,Int,Int,Int)
    testPaths [] (m,n,t) = pure (Success n,m,n,t)
    testPaths _ (m,n,t) | t > maxTimeouts = pure (GaveUp,m,n,t)
    testPaths (p:ps) (m,n,t) = do
      sat <- isSatPath solverTimeout p
      if sat -- does not account for timeouts yet
        then do
          (out,k) <- testPath p 0 n
          case out of
            PathSuccess -> testPaths ps (m+1,n+k,t)
            PathFailure i et at r -> pure (Failure i et at r,m+1,n+k,t)
            PathTimeout
              | k > 0 -> testPaths ps (m+1,n+k,t)
              | otherwise -> testPaths ps (m,n,t+1)
        else testPaths ps (m,n,t)
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
  deriving Eq

isSuccess :: Outcome -> Bool
isSuccess Success{} = True
isSuccess _ = False

data PathOutcome = PathSuccess | PathTimeout | PathFailure Inputs ExpectedRun ActualRun MatchResult
  deriving Eq

instance Show Outcome where
  show (Success n) = "Success " ++ show n
  show (Failure is et at r) = unlines ["Failure","  "++show is, "  "++show et,"  "++show at,"  "++show r]
  show GaveUp = "GaveUp"

pPrintOutcome :: Outcome -> String
pPrintOutcome (Success n) = unwords ["+++ OK, passed",show n,"tests."]
pPrintOutcome (Failure is et at r) = unlines
  [ "*** Failure"
  , "Input sequence "++ pPrintInputs is
  , "Expected run (generalized): " ++ pPrintTrace et
  , "Actual run: " ++ pPrintTrace at
  , "Error:",pPrintMatchResult r
  ]
pPrintOutcome GaveUp = "*** Gave up!"

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
