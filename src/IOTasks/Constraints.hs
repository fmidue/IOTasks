{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module IOTasks.Constraints where

import IOTasks.ValueSet
import IOTasks.Term
import IOTasks.Specification

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
constraintTree negMax =
  sem
    (\(n,e) x vs mode ->
      let
        e' = inc x e
        p = (x,e',vs)
      in case mode of
          AssumeValid -> RecSub p (n,e')
          UntilValid
            | n < negMax -> RecBoth p (n,e') (n+1,e')
            | otherwise -> RecSub p (n,e')
    )
    (\case
      RecSub (x,e',vs) s' -> Assert (InputConstraint(x, ix x e') vs) s'
      RecBoth (x,e',vs) s' s -> Choice
        (Assert (InputConstraint(x, ix x e') (complement vs)) s)
        (Assert (InputConstraint(x, ix x e') vs) s')
      NoRec -> error "constraintTree: impossible"
      RecSame{} -> error "constraintTree: impossible"
    )
    (\_ _ _ t -> t)
    (\(_,e) c l r -> Choice (Assert (ConditionConstraint c e) l) (Assert (ConditionConstraint (Not c) e) r))
    Empty
    (0,Map.empty)


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

pathDepth :: Path -> Int
pathDepth =  length . fst . partitionPath

partitionPath :: Path -> ([Constraint],[Constraint])
partitionPath = partition (\case InputConstraint{} -> True; _ -> False)

printPath :: Path -> String
printPath = intercalate ";" . map printConstraint

printConstraint :: Constraint -> String
printConstraint (InputConstraint (x,i) vs) = concat [x,"_",show i," : ",printValueSet vs]
printConstraint (ConditionConstraint t m) = printIndexedTerm t m
