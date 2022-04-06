{-# LANGUAGE RecordWildCards #-}
module Testing where

import IOrep
import Specification
import Constraints
import Trace
import Z3

import Control.Monad (forM, replicateM)
import Data.Maybe
import Data.List (nub)

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testsPerPath :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 5

fulfills :: TestConfig -> IOrep () -> Specification -> IO Outcome
fulfills TestConfig{..} prog spec  = do
  let ps = paths depth $ constraintTree spec
  nestedIs <- forM ps $ \p -> do
    ms <- replicateM testsPerPath $ findPathInput p sizeBound
    pure $ catMaybes ms
  let is = nub $ concat nestedIs
  putStrLn $ "generated " ++ show (length is) ++ " unique inputs covering " ++ show (length $ filter (not.null) nestedIs) ++ " paths."
  pure $ runTests prog spec is

type Inputs = [Integer]

data Outcome = Success | Failure Inputs MatchResult

instance Show Outcome where
  show Success = "Success"
  show (Failure is r) = unlines ["Failure","  "++show is, "  "++show r]

runTests :: IOrep () -> Specification -> [Inputs] -> Outcome
runTests _ _ [] = Success
runTests prog spec (i:is) =
  let
    specTrace = runSpecification i spec
    progTrace = runProgram i prog
  in case specTrace `covers` progTrace of
    MatchSuccessfull -> runTests prog spec is
    failure -> Failure i failure
