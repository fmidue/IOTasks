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
genTrace spec depth bound maxNeg =
  semM
    (\(e,d,n) x vs mode ->
      if d <= (0 :: Int) then pure (const OutOfInputs,(undefined,undefined))
      else do
        (set,chooseT,n') <- frequency $ (5,pure (vs,fst,n)) : [(1,pure (complement vs,snd,n+1)) | mode == UntilValid && n < maxNeg]
        i <- valueOf set bound
        let st' = (Map.update (\xs -> Just $ i:xs) x e,d-1,n')
        pure (\t' -> foldr ProgRead (chooseT t') (show i ++ "\n") ,(st',st'))
    )
    (\(e,_,_) o ts t' -> ProgWrite o (Set.map (evalPattern $ Map.toList e) ts) <$> t')
    (\(e,_,_) c l r -> if eval c $ Map.toList e then l else r)
    (pure Terminate)
    (Map.fromList ((,[]) <$> vars spec),depth,0)
    spec

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
