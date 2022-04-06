{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
module Specification where

import ValueSet
import Term
import Trace

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub)
import qualified Data.Map as Map

data Specification where
  ReadInput :: Varname -> ValueSet -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (Term Integer) -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  Until :: Term Bool -> Specification -> Specification -> Specification

instance Semigroup Specification where
  s <> Nop = s
  Nop <> s = s
  (ReadInput x vs s) <> s' = ReadInput x vs $ s <> s'
  (WriteOutput o t s) <> s' = WriteOutput o t $ s <> s'
  (Branch c l r s) <> s' = Branch c l r $ s <> s'
  (Until cond body s) <> s' = Until cond body $ s <> s'

instance Monoid Specification where
  mempty = Nop

readInput :: Varname -> ValueSet -> Specification
readInput x vs = ReadInput x vs nop

writeOutput :: [Term Integer] -> Specification
writeOutput ts = WriteOutput Mandatory (Set.fromList ts) nop

writeOptionalOutput :: [Term Integer] -> Specification
writeOptionalOutput ts = WriteOutput Optional (Set.fromList ts) nop

branch :: Term Bool -> Specification -> Specification -> Specification
branch c t e = Branch c t e nop

nop :: Specification
nop = Nop

until :: Term Bool -> Specification -> Specification
until c bdy = Until c bdy nop

vars :: Specification -> [Varname]
vars = nub . go where
  go (ReadInput x _ s') = x : go s'
  go (WriteOutput _ _ s') = go s'
  go (Branch _ l r s') = go l ++ go r ++ go s'
  go Nop = []
  go (Until _ bdy s') = go bdy ++ go s'

runSpecification :: [Integer] -> Specification -> Trace
runSpecification inputs spec = runSpecification' (Map.fromList ((,[]) <$> vars spec)) inputs spec where
  runSpecification' :: Map.Map Varname [Integer] -> [Integer] -> Specification -> Trace
  runSpecification' _ [] ReadInput{} = OutOfInputs
  runSpecification' e (i:is) (ReadInput x vs s')
    | vs `containsValue` i = ProgRead i $ runSpecification' (Map.update (\xs -> Just $ i:xs) x e) is s'
    | otherwise = error "invalid value"
  runSpecification' e is (WriteOutput o ts s') = ProgWrite o (Set.map (`eval` Map.toList e) ts) $ runSpecification' e is s'
  runSpecification' e is (Branch c l r s')
    | eval c $ Map.toList e = runSpecification' e is $ l <> s'
    | otherwise = runSpecification' e is $ r <> s'
  runSpecification' _ _ Nop = Terminate
  runSpecification' e is s@(Until c bdy s')
    | eval c $ Map.toList e = runSpecification' e is s'
    | otherwise = runSpecification' e is $ bdy <> s
