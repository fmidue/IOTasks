{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Test.IOTasks.Constraints (
  Constraint(..), SomeConstraint(..),
  ConstraintType(..),
  ConstraintTree(..),
  constraintTree,
  Path, paths,
  partitionPath, pathDepth,
  numberOfPaths,
  printPath, printConstraint, printSomeConstraint,
  ) where

import Test.IOTasks.ValueSet
import Test.IOTasks.ConditionTerm (ConditionTerm, printIndexedTerm, castTerm, subTerms)
import Test.IOTasks.Terms (Var, SomeVar, someVar, not', varname)
import Test.IOTasks.Internal.Specification
import Test.IOTasks.OutputPattern (valueTerms)
import Test.IOTasks.OutputTerm (transparentSubterms, withSomeOutputTerm)

import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set (toList)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tuple.Extra (fst3)
import Data.Typeable

data Constraint (t :: ConstraintType) where
  InputConstraint :: Typeable v => (Var v, Int) -> ValueSet v -> Constraint 'Input
  ConditionConstraint :: ConditionTerm Bool -> Map SomeVar (Int, [Int]) -> Constraint 'Condition
  OverflowConstraints :: [ConditionTerm Integer] -> Map SomeVar (Int, [Int]) -> Constraint 'Overflow

data ConstraintType = Input | Condition | Overflow

data SomeConstraint where
  SomeConstraint :: Constraint t -> SomeConstraint

data ConstraintTree where
  Choice :: ConstraintTree -> ConstraintTree -> ConstraintTree
  Assert :: (Constraint t) -> ConstraintTree -> ConstraintTree
  Unfold :: ConstraintTree -> ConstraintTree -- marker for counting path lengths
  Empty :: ConstraintTree

constraintTree :: Int -> Specification -> ConstraintTree
constraintTree negMax =
  sem
    (\(n,e,k) x vs mode ->
      let
        e' = inc (someVar x) k e
        p = (InputConstraint(x, ix (someVar x) e') vs
            ,InputConstraint(x, ix (someVar x) e') (complement vs)
            , mode == Abort && n < negMax)
      in case mode of
          AssumeValid -> RecSub p id (n,e',k+1)
          UntilValid
            | n < negMax -> RecBoth p id (n,e',k) (n+1,e',k+1)
            | otherwise -> RecSub p id (n,e',k+1)
          Abort -> RecSub p id (n,e',k)
    )
    (\case
      RecSub (vsP,_, False) () s' -> Assert vsP s'
      RecSub (vsP,vsN, True) () s' -> Choice
        (Assert vsN Empty)
        (Assert vsP s')
      RecBoth (vsP,vsN,_) () s' s -> Choice
        (Assert vsN s)
        (Assert vsP s')
      NoRec _ -> error "constraintTree: impossible"
      RecSame{} -> error "constraintTree: impossible"
    )
    (\(_,e,_) _ ps t -> Assert (OverflowConstraints (catMaybes $ [ castTerm @Integer t | p <- Set.toList ps, vt <- valueTerms p, t <- withSomeOutputTerm vt transparentSubterms]) e) t)
    (\(_,e,_) c l r -> Assert (OverflowConstraints (mapMaybe (castTerm @Integer) $ subTerms c) e) $ Choice (Assert (ConditionConstraint c e) l) (Assert (ConditionConstraint (not' c) e) r))
    (\case {End -> Unfold ; Exit -> id})
    Empty
    (0,Map.empty,1)

ix :: SomeVar -> Map SomeVar (Int,a) -> Int
ix x m = maybe 0 fst (Map.lookup x m)

inc :: SomeVar -> Int -> Map SomeVar (Int,[Int]) -> Map SomeVar (Int,[Int])
inc x k m
  | x `elem` Map.keys m = Map.update (\(c,ks) -> Just (c + 1,k:ks)) x m
  | otherwise = Map.insert x (1,[k]) m

type Path = [SomeConstraint]

-- paths n t returns all terminating paths in t that use at most n unfoldings of iterations
paths :: Int -> ConstraintTree -> [Path]
paths _ Empty = [[]]
paths n _ | n < 0 = []
paths n (Choice lt rt) = mergePaths (paths n lt) (paths n rt)
paths n (Assert c t) = (SomeConstraint c:) <$> paths n t
paths n (Unfold t)
  | n > 0 = paths (n-1) t
  | otherwise = []

mergePaths :: [Path] -> [Path] -> [Path]
mergePaths xs [] = xs
mergePaths [] ys = ys
mergePaths (x:xs) (y:ys) = x:y:mergePaths xs ys

pathDepth :: Path -> Int
pathDepth =  length . fst3 . partitionPath

partitionPath :: Path -> ([Constraint 'Input],[Constraint 'Condition],[Constraint 'Overflow])
partitionPath = foldMap phi where
  phi :: SomeConstraint -> ([Constraint 'Input],[Constraint 'Condition],[Constraint 'Overflow])
  phi (SomeConstraint c@InputConstraint{}) = ([c],mempty,mempty)
  phi (SomeConstraint c@ConditionConstraint{}) = (mempty,[c],mempty)
  phi (SomeConstraint c@OverflowConstraints{}) = (mempty,mempty,[c])

printPath :: Path -> String
printPath = unlines . intersperse " |" . map printSomeConstraint

printSomeConstraint :: SomeConstraint -> String
printSomeConstraint (SomeConstraint c) = printConstraint c

printConstraint :: Constraint t -> String
printConstraint (InputConstraint (x,i) vs) = concat [varname x,"_",show i," : ",printValueSet vs]
printConstraint (ConditionConstraint t m) = printIndexedTerm t m
printConstraint (OverflowConstraints _ _) = "**some overflow checks**"

numberOfPaths :: Integer -> Specification -> Integer
numberOfPaths maxIterationDepth s =
  case go s maxIterationDepth of
    (i,0) -> i
    _ -> error "numberOfPaths: unbound top-level exit marker"
  where
  go :: Specification -> Integer -> (Integer, Integer)
  go (ReadInput _ _  _ s') n = go s' n
  go (WriteOutput _ _ s') n = go s' n
  go (Branch _ t e s') n =
    let
      (it,kt) = go t n
      (ie,ke) = go e n
      (i,k) = go s' n
    in ((it+ie)*i,kt+ke+k)
  go (TillE bdy s') n
    | hasIteration bdy = error "numberOfPaths: can't compute number of paths for nested iterations, yet"
    | hasIteration s' =
      let
        rs = do
          (n1,n2) <- [ (n1,n-n1) | n1 <- [0..n] ]
          let
            (ib, kb) = go bdy n1
            (i,k) = go s' n2
          pure (p' ib kb n1 *i,k)
      in foldr (\(i,k) (si,sk) -> (i+si,k+sk)) (0,0) rs
    | otherwise =
      let
        (ib,kb) = go bdy n
        (i,k) = go s' n
      in (p ib kb n *i,k)
  go Nop _ = (1,0)
  go E _ = (0,1)
  -- path with length (<=n) in an iteration with i paths to Nop and k paths to E under the iteration
  p :: Integer -> Integer -> Integer -> Integer
  p 0 k _ = k
  p 1 k n = k * (n + 1)
  p i k n = (k * (i ^ (n+1) - 1)) `div` (i - 1)
  -- path with length (=n) in an iteration with i paths to Nop and k paths to E under the iteration
  p' :: Integer -> Integer -> Integer -> Integer
  p' i k n = k * (i ^ n)
