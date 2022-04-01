{-# LANGUAGE RecordWildCards #-}
module Testing where

import IOrep
import Specification
import Constraints
import Z3

import Control.Monad (forM, replicateM)
import Data.Maybe
import Data.List (nub)

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testsPerPath :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 5

fulfills :: TestConfig -> IOrep () -> Specification -> IO Bool
fulfills TestConfig{..} prog spec  = do
  let ps = paths depth $ constraintTree spec
  nestedIs <- forM ps $ \p -> do
    ms <- replicateM testsPerPath $ findPathInput p sizeBound
    pure $ catMaybes ms
  let is = nub $ concat nestedIs
  putStrLn $ "generated " ++ show (length is) ++ " unique inputs covering " ++ show (length $ filter (not.null) nestedIs) ++ " paths."  
  pure $ all (\i -> runProgram i prog == runSpecification i spec) is
