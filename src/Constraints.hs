{-# LANGUAGE LambdaCase #-}
module Constraints where

import ValueSet
import Term
import Specification

import Data.List (partition)
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

constraintTree :: Specification -> ConstraintTree
constraintTree = constraintTree' Map.empty where
  constraintTree' e (ReadInput x vs s) = Assert (InputConstraint(x, ix x e) vs) $ constraintTree' (inc x e) s
  constraintTree' e (WriteOutput _ s) = constraintTree' e s
  constraintTree' e (Branch c l r s) =
    Choice (Assert (ConditionConstraint c e) $ constraintTree' e (l <> s)) (Assert (ConditionConstraint (Not c) e) $ constraintTree' e (r <> s))
  constraintTree' _ Nop = Empty

ix :: Varname -> Map Varname Int -> Int
ix x m = fromMaybe 0 $ Map.lookup x m

inc :: Varname -> Map Varname Int -> Map Varname Int
inc x m
  | x `elem` Map.keys m = Map.update (\c -> Just $ c + 1) x m
  | otherwise = Map.insert x 0 m

type Path = [Constraint]

paths :: Int -> ConstraintTree -> [Path]
paths _ Empty = [[]]
paths 0 _ = []
paths n (Choice lt rt) = paths n lt ++ paths n rt
paths n (Assert c t) = (c:) <$> paths (n-1) t

partitionPath :: Path -> ([Constraint],[Constraint])
partitionPath = partition (\case InputConstraint{} -> True; _ -> False)
