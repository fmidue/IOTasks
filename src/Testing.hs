{-# LANGUAGE RecordWildCards #-}
module Testing where

import IOrep (IOrep, Line, runProgram)
import Specification
import Constraints
import Trace
import Z3

import Control.Monad (forM, replicateM, filterM)
import Data.Maybe
import Data.List (nub)

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testsPerPath :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 5

fulfills :: TestConfig -> IOrep () -> Specification -> IO Outcome
fulfills TestConfig{..} prog spec  = do
  let ps = paths depth $ constraintTree spec
  satPaths <- filterM isSatPath ps
  nestedIs <- forM satPaths $ \p -> do
    ms <- replicateM testsPerPath $ findPathInput p sizeBound
    pure $ catMaybes ms
  let is = map (map show) $ nub $ concat nestedIs
  putStrLn $ unwords ["generated", show (length is),"unique inputs covering", show (length satPaths),"satisfiable paths ("++ show (length ps),"paths with max. depth",show depth,"in total)"]
  pure . fst $ runTests prog spec is

type Inputs = [Line]

data Outcome = Success | Failure Inputs MatchResult
  deriving Eq

instance Show Outcome where
  show Success = "Success"
  show (Failure is r) = unlines ["Failure","  "++show is, "  "++show r]

runTests :: IOrep () -> Specification -> [Inputs] -> (Outcome,Int)
runTests = go 0 where
  go n _ _ [] = (Success,n)
  go n prog spec (i:is) =
    let
      specTrace = runSpecification i spec
      progTrace = runProgram i prog
    in case specTrace `covers` progTrace of
      MatchSuccessfull -> go (n+1) prog spec is
      failure -> (Failure i failure,n)
