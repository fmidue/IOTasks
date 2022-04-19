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
  ReadInput :: Varname -> ValueSet -> InputMode -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (Term Integer) -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  Until :: Term Bool -> Specification -> Specification -> Specification

data InputMode = AssumeValid | UntilValid deriving (Eq,Show)

instance Semigroup Specification where
  s <> Nop = s
  Nop <> s = s
  (ReadInput x vs m s) <> s' = ReadInput x vs m $ s <> s'
  (WriteOutput o t s) <> s' = WriteOutput o t $ s <> s'
  (Branch c l r s) <> s' = Branch c l r $ s <> s'
  (Until cond body s) <> s' = Until cond body $ s <> s'

instance Monoid Specification where
  mempty = Nop

readInput :: Varname -> ValueSet -> InputMode -> Specification
readInput x vs m = ReadInput x vs m nop

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
  go (ReadInput x _ _ s') = x : go s'
  go (WriteOutput _ _ s') = go s'
  go (Branch _ l r s') = go l ++ go r ++ go s'
  go Nop = []
  go (Until _ bdy s') = go bdy ++ go s'

runSpecification :: [String] -> Specification -> Trace
runSpecification inputs spec = runSpecification' (Map.fromList ((,[]) <$> vars spec)) inputs spec where
  runSpecification' :: Map.Map Varname [Integer] -> [String] -> Specification -> Trace
  runSpecification' _ [] ReadInput{} = OutOfInputs
  runSpecification' e (i:is) s@(ReadInput x vs mode s')
    | vs `containsValue` read i = foldr ProgRead (ProgRead '\n' $ runSpecification' (Map.update (\xs -> Just $ read i:xs) x e) is s') i
    | otherwise = case mode of
        AssumeValid -> error "invalid value"
        UntilValid -> foldr ProgRead (ProgRead '\n' $ runSpecification' e is s) i
  runSpecification' e is (WriteOutput o ts s') = ProgWrite o (Set.map ((++"\n"). show . (`eval` Map.toList e)) ts) $ runSpecification' e is s'
  runSpecification' e is (Branch c l r s')
    | eval c $ Map.toList e = runSpecification' e is $ l <> s'
    | otherwise = runSpecification' e is $ r <> s'
  runSpecification' _ _ Nop = Terminate
  runSpecification' e is s@(Until c bdy s')
    | eval c $ Map.toList e = runSpecification' e is s'
    | otherwise = runSpecification' e is $ bdy <> s
