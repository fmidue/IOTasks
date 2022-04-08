{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Testing.Random where

import Testing hiding (fulfills, TestConfig)

import Data.Map as Map hiding (foldr)
import Data.Set as Set hiding (foldr)

import IOrep (IOrep, runProgram)
import Specification
import Trace
import Term
import OutputPattern

import Test.QuickCheck (Gen, vectorOf, generate, frequency)
import ValueSet

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testCases :: Int, maxNegativeInputs :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 100 5

fulfills :: TestConfig -> IOrep () -> Specification -> IO Outcome
fulfills TestConfig{..} prog spec  = do
  is <- generate $ vectorOf testCases $ genInput spec depth sizeBound maxNegativeInputs
  let (outcome, n) = runTests prog spec is
  putStrLn $ unwords ["passed", show n,"tests"]
  pure outcome

genInput :: Specification -> Int -> Integer -> Int -> Gen Inputs
genInput s depth bound maxNeg = do
  t <- genTrace s depth bound maxNeg
  if isTerminating t
    then pure $ inputSequence t
    else genInput s depth bound maxNeg -- repeat sampling until a terminating trace is found

genTrace :: Specification -> Int -> Integer -> Int -> Gen Trace
genTrace spec depth bound maxNeg = genTrace' (Map.fromList ((,[]) <$> vars spec)) depth 0 spec where
  genTrace' :: Map.Map Varname [Integer] -> Int -> Int -> Specification -> Gen Trace
  genTrace' e d n s@(ReadInput x vs mode s')
    | d <= 0 = pure OutOfInputs
    | otherwise = do
      (set,nextSpec,n') <- frequency $ (5,pure (vs,s',n)) : [(1,pure (complement vs,s,n+1)) | mode == UntilValid && n < maxNeg]
      i <- valueOf set bound
      t' <- genTrace' (Map.update (\xs -> Just $ i:xs) x e) (d-1) n' nextSpec
      pure $ foldr ProgRead t' (show i ++ "\n")

  genTrace' e d n (WriteOutput o ts s') =
    do
      t' <- genTrace' e d n s'
      pure $ ProgWrite o (Set.map (evalPattern $ Map.toList e) ts) t'
  genTrace' e d n (Branch c l r s')
    | eval c $ Map.toList e = genTrace' e d n $ l <> s'
    | otherwise = genTrace' e d n $ r <> s'
  genTrace' _ _ _ Nop = pure Terminate
  genTrace' e d n s@(Until c bdy s')
    | eval c $ Map.toList e = genTrace' e d n s'
    | otherwise = genTrace' e d n $ bdy <> s

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
