{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Constraints where

import ValueSet
import Term
import Specification

import Data.List (partition,intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)

data Constraint
  = InputConstraint (Varname,Int) ValueSet
  | ConditionConstraint (Term Bool) (Map Varname Int)

data ConstraintTree
  = Choice ConstraintTree ConstraintTree
  | Assert Constraint ConstraintTree
  | Empty

constraintTree :: Int -> Specification -> ConstraintTree
constraintTree negMax = constraintTree' 0 Map.empty where

  constraintTree' n e s@(ReadInput x vs mode s') =
    let
      e' = inc x e
      modeContext = case mode of
        AssumeValid -> id
        UntilValid
          | n < negMax -> Choice
            (Assert (InputConstraint(x, ix x e') (complement vs)) $ constraintTree' (n+1) e' s)
          | otherwise -> id
    in
      modeContext $ Assert (InputConstraint(x, ix x e') vs) $ constraintTree' n e' s'

  constraintTree' n e (WriteOutput _ _ s) = constraintTree' n e s
  constraintTree' n e (Branch c l r s) =
    Choice (Assert (ConditionConstraint c e) $ constraintTree' n e (l <> s)) (Assert (ConditionConstraint (Not c) e) $ constraintTree' n e (r <> s))
  constraintTree' _ _ Nop = Empty
  constraintTree' n e loop@(Until c body s) = Choice (Assert (ConditionConstraint c e) $ constraintTree' n e s) (Assert (ConditionConstraint (Not c) e) $ constraintTree' n e $ body <> loop)

ix :: Varname -> Map Varname Int -> Int
ix x m = fromMaybe 0 $ Map.lookup x m

inc :: Varname -> Map Varname Int -> Map Varname Int
inc x m
  | x `elem` Map.keys m = Map.update (\c -> Just $ c + 1) x m
  | otherwise = Map.insert x 1 m

type Path = [Constraint]

paths :: Int -> ConstraintTree -> [Path]
paths _ Empty = [[]]
paths n _ | n < 0 = []
paths n (Choice lt rt) = paths n lt ++ paths n rt
paths n (Assert c@InputConstraint{} t) = (c:) <$> paths (n-1) t
paths n (Assert c@ConditionConstraint{} t) = (c:) <$> paths n t

partitionPath :: Path -> ([Constraint],[Constraint])
partitionPath = partition (\case InputConstraint{} -> True; _ -> False)

printPath :: Path -> String
printPath = intercalate ";" . map printConstraint

printConstraint :: Constraint -> String
printConstraint (InputConstraint (x,i) vs) = concat [x,"_",show i," : ",printValueSet vs]
printConstraint (ConditionConstraint t m) = printIndexedTerm t m
