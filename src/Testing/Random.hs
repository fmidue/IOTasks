{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Testing.Random where

import Testing hiding (fulfills, TestConfig)

import Data.Map as Map hiding (foldr)
import Data.Set as Set hiding (foldr)

import IOrep
import Specification
import Trace
import Term

import Test.QuickCheck (Gen, vectorOf, generate)
import ValueSet

data TestConfig = TestConfig { depth :: Int, sizeBound :: Integer, testCases :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig 25 100 100

fulfills :: TestConfig -> IOrep () -> Specification -> IO Outcome
fulfills TestConfig{..} prog spec  = do
  is <- generate $ vectorOf testCases $ genInput spec depth sizeBound
  pure $ runTests prog spec is

genInput :: Specification -> Int -> Integer -> Gen Inputs
genInput s depth bound = do
  t <- genTrace s depth bound
  if isTerminating t
    then pure $ inputSequence t
    else genInput s depth bound -- repeat sampling until a terminating trace is found

genTrace :: Specification -> Int -> Integer -> Gen Trace
genTrace spec depth bound = genTrace' (Map.fromList ((,[]) <$> vars spec)) depth spec where
  genTrace' :: Map.Map Varname [Integer] -> Int -> Specification -> Gen Trace
  genTrace' e d (ReadInput x vs s')
    | d <= 0 = pure OutOfInputs
    | otherwise = do
      i <- valueOf vs bound
      t' <- genTrace' (Map.update (\xs -> Just $ i:xs) x e) (d-1) s'
      pure $ foldr ProgRead t' (show i)
  genTrace' e d (WriteOutput o ts s') =
    do
      t' <- genTrace' e d s'
      pure $ ProgWrite o (Set.map (show . (`eval` Map.toList e)) ts) t'
  genTrace' e d (Branch c l r s')
    | eval c $ Map.toList e = genTrace' e d $ l <> s'
    | otherwise = genTrace' e d $ r <> s'
  genTrace' _ _ Nop = pure Terminate
  genTrace' e d s@(Until c bdy s')
    | eval c $ Map.toList e = genTrace' e d s'
    | otherwise = genTrace' e d $ bdy <> s
