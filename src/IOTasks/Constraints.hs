{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module IOTasks.Constraints where

import IOTasks.ValueSet
import IOTasks.Term
import IOTasks.Specification

import Data.List (partition,intercalate)
import qualified Data.Map as Map
import Data.Map (Map)

data Constraint
  = InputConstraint (Varname,Int) ValueSet
  | ConditionConstraint (Term Bool) (Map Varname (Int,[Int]))

data ConstraintTree
  = Choice ConstraintTree ConstraintTree
  | Assert Constraint ConstraintTree
  | Empty

constraintTree :: Int -> Specification -> ConstraintTree
constraintTree negMax =
  sem
    (\(n,e,k) x vs mode ->
      let
        e' = inc x k e
        p = (x,e',vs, mode == Abort && n < negMax)
      in case mode of
          AssumeValid -> RecSub p (n,e',k+1)
          UntilValid
            | n < negMax -> RecBoth p (n,e',k) (n+1,e',k+1)
            | otherwise -> RecSub p (n,e',k+1)
          Abort -> RecSub p (n,e',k)
    )
    (\case
      RecSub (x,e',vs, False) s' -> Assert (InputConstraint(x, ix x e') vs) s'
      RecSub (x,e',vs, True) s' -> Choice
        (Assert (InputConstraint(x, ix x e') (complement vs)) Empty)
        (Assert (InputConstraint(x, ix x e') vs) s')
      RecBoth (x,e',vs,_) s' s -> Choice
        (Assert (InputConstraint(x, ix x e') (complement vs)) s)
        (Assert (InputConstraint(x, ix x e') vs) s')
      NoRec _ -> error "constraintTree: impossible"
      RecSame{} -> error "constraintTree: impossible"
    )
    (\_ _ _ t -> t)
    (\(_,e,_) c l r -> Choice (Assert (ConditionConstraint c e) l) (Assert (ConditionConstraint (Not c) e) r))
    Empty
    (0,Map.empty,1)


ix :: Varname -> Map Varname (Int,a) -> Int
ix x m = maybe 0 fst (Map.lookup x m)

inc :: Varname -> Int -> Map Varname (Int,[Int]) -> Map Varname (Int,[Int])
inc x k m
  | x `elem` Map.keys m = Map.update (\(c,ks) -> Just (c + 1,k:ks)) x m
  | otherwise = Map.insert x (1,[k]) m

type Path = [Constraint]

paths :: Int -> ConstraintTree -> [Path]
paths _ Empty = [[]]
paths n _ | n < 0 = []
paths n (Choice lt rt) = paths n lt ++ paths n rt
paths n (Assert c@InputConstraint{} t) = (c:) <$> paths (n-1) t
paths n (Assert c@ConditionConstraint{} t) = (c:) <$> paths n t

pathDepth :: Path -> Int
pathDepth =  length . fst . partitionPath

partitionPath :: Path -> ([Constraint],[Constraint])
partitionPath = partition (\case InputConstraint{} -> True; _ -> False)

printPath :: Path -> String
printPath = intercalate ";" . map printConstraint

printConstraint :: Constraint -> String
printConstraint (InputConstraint (x,i) vs) = concat [x,"_",show i," : ",printValueSet vs]
printConstraint (ConditionConstraint t m) = printIndexedTerm t m
