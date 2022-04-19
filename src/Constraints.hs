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

constraintTree :: Specification -> ConstraintTree
constraintTree = constraintTree' Map.empty where

  constraintTree' e s@(ReadInput x vs mode s') =
    let
      e' = inc x e
      modeContext = case mode of
        AssumeValid -> id
        UntilValid -> Choice
          (Assert (InputConstraint(x, ix x e') (complement vs)) $ constraintTree' e' s)

    in
      modeContext $ Assert (InputConstraint(x, ix x e') vs) $ constraintTree' e' s'

  constraintTree' e (WriteOutput _ _ s) = constraintTree' e s
  constraintTree' e (Branch c l r s) =
    Choice (Assert (ConditionConstraint c e) $ constraintTree' e (l <> s)) (Assert (ConditionConstraint (Not c) e) $ constraintTree' e (r <> s))
  constraintTree' _ Nop = Empty
  constraintTree' e loop@(Until c body s) = Choice (Assert (ConditionConstraint c e) $ constraintTree' e s) (Assert (ConditionConstraint (Not c) e) $ constraintTree' e $ body <> loop)

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

printIndexedTerm :: Term a -> Map Varname Int -> String
printIndexedTerm (tx :+: ty) m = concat ["(",printIndexedTerm tx m, ") + (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :-: ty) m = concat ["(",printIndexedTerm tx m, ") - (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :*: ty) m = concat ["(",printIndexedTerm tx m, ") * (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :==: ty) m = concat ["(",printIndexedTerm tx m, ") == (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>: ty) m = concat ["(",printIndexedTerm tx m, ") > (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>=: ty) m = concat ["(",printIndexedTerm tx m, ") >= (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<: ty) m = concat ["(",printIndexedTerm tx m, ") < (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<=: ty) m = concat ["(",printIndexedTerm tx m, ") <= (", printIndexedTerm ty m,")"]
printIndexedTerm (Not t) m = concat ["not (", printIndexedTerm t m, ")"]
printIndexedTerm (tx :&&: ty) m = concat ["(",printIndexedTerm tx m, ") && (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :||: ty) m = concat ["(",printIndexedTerm tx m, ") || (", printIndexedTerm ty m,")"]
printIndexedTerm (Length t) m = concat ["length (", printIndexedTerm t m, ")"]
printIndexedTerm (Sum t) m = concat ["sum (", printIndexedTerm t m, ")"]
printIndexedTerm (Product t) m = concat ["product (", printIndexedTerm t m, ")"]
printIndexedTerm (Current x) m = concat [x,"_",show $ ix x m]
printIndexedTerm (All x) _ = x++"_A"
printIndexedTerm (IntLit x) _ = show x
