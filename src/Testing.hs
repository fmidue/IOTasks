{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Testing where

import IOrep (IOrep, Line, runProgram)
import Specification
import Constraints
import Trace
import Z3

import Control.Monad (when)

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testsPerPath :: Int, maxNegativeInputs :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 5 5

fulfills :: TestConfig -> IOrep () -> Specification -> IO Outcome
fulfills TestConfig{..} prog spec = do
  let ps = paths depth $ constraintTree maxNegativeInputs spec
  (out,satPaths,nInputs,timeouts) <- testPaths ps (0,0,0)
  putStrLn $ unwords
    ["generated", show nInputs,"inputs covering", show satPaths,"satisfiable paths ("++ show (length ps),"paths with max. depth",show depth,"in total)"]
  when (timeouts > 0) $ putStrLn $ unwords ["---",show timeouts, "paths timed out"]
  pure out

  where
    testPaths :: [Path] -> (Int,Int,Int) -> IO (Outcome,Int,Int,Int)
    testPaths [] (m,n,t) = pure (Success,m,n,t)
    testPaths (p:ps) (m,n,t) = do
      sat <- isSatPath p
      if sat -- does not account for timeouts yet
        then do
          (out,k) <- testPath p 0 n
          case out of
            PathSuccess -> testPaths ps (m+1,n+k,t)
            PathFailure i r -> pure (Failure i r,m+1,n+k,t)
            PathTimeout
              | k > 0 -> testPaths ps (m+1,n+k,t)
              | otherwise -> testPaths ps (m,n,t+1)
        else testPaths ps (m,n,t)
    testPath :: Path -> Int -> Int -> IO (PathOutcome,Int)
    testPath _ n _ | n >= testsPerPath = pure (PathSuccess,n)
    testPath p n nOtherTests = do
      mNextInput <- fmap @Maybe (map show) <$> findPathInput p sizeBound
      case mNextInput of
        Nothing -> pure (PathTimeout,n) -- should (only?) be the case if solving times out
        Just nextInput  -> do
          putStr $ concat ["(",show (n+nOtherTests)," tests)\r"]
          let
            specTrace = runSpecification nextInput spec
            progTrace = runProgram nextInput prog
          case specTrace `covers` progTrace of
            MatchSuccessfull -> testPath p (n+1) nOtherTests
            failure -> pure (PathFailure nextInput failure,n+1)

type Inputs = [Line]

data Outcome = Success | Failure Inputs MatchResult
  deriving Eq

data PathOutcome = PathSuccess | PathTimeout | PathFailure Inputs MatchResult
  deriving Eq

instance Show Outcome where
  show Success = "Success"
  show (Failure is r) = unlines ["Failure","  "++show is, "  "++show r]
