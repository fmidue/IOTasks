{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTasks.Constraints (
  SimpleConstraint(..),
  Constraint(..),
  ConstraintType(..),
  ConstraintTree(..),
  constraintTree,
  SimplePath(..), simplePaths,
  appendPath, addEnvToPath, canBeInjected, injectionPoints,
  Path, SymbolicInfo(..),
  storedValues,
  completePath, partitionPath,
  pathDepth,
  numberOfPaths,
  showSimplePath, showSimpleConstraint, showSomeSimpleConstraint,
  showPath, showConstraint, showSomeConstraint,
  ix
  ) where

import Test.IOTasks.ValueSet
import Test.IOTasks.Internal.Term
import Test.IOTasks.Var (Var, SomeVar, someVar, varname)
import Test.IOTasks.Term.Prelude (not')
import Test.IOTasks.Internal.Specification
import Test.IOTasks.OutputPattern (valueTerms)

import Data.List (intersperse, sort)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set (toList)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Tuple.Extra (fst3)
import Data.Typeable
import Data.Some
import Test.QuickCheck

data SimpleConstraint (t :: ConstraintType) where
  SimpleInput :: Typeable v => StorageType -> Var v -> ValueSet v -> SimpleConstraint 'Input
  SimpleCondition :: Term 'Transparent Bool -> SimpleConstraint 'Condition
  SimpleOverflow :: [Term 'Transparent Integer] -> SimpleConstraint 'Overflow

data StorageType = KeepInput | DropInput
  deriving Show

data Constraint (t :: ConstraintType) where
  InputConstraint :: Typeable v => (Var v, Int) -> ValueSet v -> Map SomeVar SymbolicInfo -> Constraint 'Input
  ConditionConstraint :: Term 'Transparent Bool -> Map SomeVar SymbolicInfo -> Constraint 'Condition
  OverflowConstraints :: [Term 'Transparent Integer] -> Map SomeVar SymbolicInfo -> Constraint 'Overflow

data ConstraintType = Input | Condition | Overflow

data ConstraintTree where
  Choice :: ConstraintTree -> ConstraintTree -> ConstraintTree
  Assert :: (SimpleConstraint t) -> ConstraintTree -> ConstraintTree
  Unfold :: ConstraintTree -> ConstraintTree -- marker for counting path lengths
  InjectionPoint :: (SimpleConstraint 'Input) -> ConstraintTree -> ConstraintTree  -- position for injecting the the given constraint 0 to n many times
  Empty :: ConstraintTree

constraintTree :: Specification -> ConstraintTree
constraintTree =
  sem
    (\() x vs mode ->
      let
        p = (SimpleInput KeepInput x vs
            ,SimpleInput DropInput x (complement vs)
            , mode)
      in RecSub p id ()
    )
    (\case
      RecSub (vsP,_, AssumeValid) () s' -> Assert vsP s'
      RecSub (vsP,vsN, UntilValid) () s' -> InjectionPoint vsN $ Assert vsP s'
      RecSub (vsP,vsN, ElseAbort) () s' -> Choice
        (Assert vsN Empty)
        (Assert vsP s')
      RecBoth{} -> error "constraintTree: impossible"
      NoRec _ -> error "constraintTree: impossible"
      RecSame{} -> error "constraintTree: impossible"
    )
    (\() _ ps t -> Assert (SimpleOverflow (catMaybes $ [ castTerm @Integer t | p <- Set.toList ps, vt <- valueTerms p, t <- withSomeTermK vt transparentSubterms])) t)
    (\() c l r -> Assert (SimpleOverflow (mapMaybe (castTerm @Integer) $ transparentSubterms c)) $ Choice (Assert (SimpleCondition c) l) (Assert (SimpleCondition (not' c)) r))
    (\case {End -> Unfold ; Exit -> id})
    Unfold
    Empty
    ()

data SimplePath where
  ClosedPath :: [Some SimpleConstraint] -> SimplePath
  OpenPath :: [Some SimpleConstraint] -> (SimpleConstraint t) -> SimplePath -> SimplePath

prependPath :: Some SimpleConstraint -> SimplePath -> SimplePath
prependPath c (ClosedPath cs) = ClosedPath (c:cs)
prependPath c (OpenPath cs ic t) = OpenPath (c:cs) ic t

appendPath :: SimplePath -> SimplePath -> SimplePath
appendPath (ClosedPath xs) (ClosedPath ys) = ClosedPath $ xs ++ ys
appendPath (ClosedPath xs) (OpenPath ys cy y) = OpenPath (xs ++ ys) cy y
appendPath (OpenPath xs cx x) y = OpenPath xs cx $ appendPath x y

canBeInjected :: SimplePath -> Bool
canBeInjected ClosedPath{} = False
canBeInjected OpenPath{} = True

-- paths n t returns all terminating paths in t that use at most n unfoldings of iterations
simplePaths :: Int -> ConstraintTree -> [SimplePath]
simplePaths _ Empty = [ClosedPath []]
simplePaths n _ | n < 0 = []
simplePaths n (Choice lt rt) = mergePaths (simplePaths n lt) (simplePaths n rt)
simplePaths n (Assert c t) = prependPath (Some c) <$> simplePaths n t
simplePaths n (InjectionPoint c t) = OpenPath [] c <$> simplePaths n t
simplePaths n (Unfold t)
  | n > 0 = simplePaths (n-1) t
  | otherwise = []

mergePaths :: [SimplePath] -> [SimplePath] -> [SimplePath]
mergePaths xs [] = xs
mergePaths [] ys = ys
mergePaths (x:xs) (y:ys) = x:y:mergePaths xs ys

type Path = [Some Constraint]

type NumberOfInjectionPoints = Int
type UsedInjectionPoints = [Int]

completePath :: [Int] -> SimplePath -> Path
completePath = addEnvToPath .: go 1 where
  go _ _ (ClosedPath cs) = cs
  go n [] (OpenPath cs _ p) = cs ++ go (n+1) [] p
  go n (i:is) (OpenPath cs c p)
    | n == i = cs ++ (Some c: go n is (OpenPath [] c p))
    | otherwise = cs ++ go (n+1) (i:is) p

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

injectionPoints :: SimplePath -> Int
injectionPoints (ClosedPath _) = 0
injectionPoints (OpenPath _ _ p) = 1 + injectionPoints p

addEnvToPath :: [Some SimpleConstraint] -> Path
addEnvToPath = snd . foldl (\((e,k),p) (Some c) -> let (e',c') = addEnvToConstraint e k c in ((e',k+1), p ++ [Some c']) ) ((Map.empty,1),[])

addEnvToConstraint :: Map SomeVar SymbolicInfo -> Int -> SimpleConstraint t -> (Map SomeVar SymbolicInfo, Constraint t)
addEnvToConstraint e k (SimpleInput ty x vs) = (e', InputConstraint (x,ix (someVar x) e') vs e')
  where
    e' = inc ty (someVar x) k e
addEnvToConstraint e _ (SimpleCondition c) = (e, ConditionConstraint c e)
addEnvToConstraint e _ (SimpleOverflow ts) = (e, OverflowConstraints ts e)

data SymbolicInfo = SymbolicInfo
  { currentIndex :: Int
  , negativeIndices :: [Int]
  , chronology :: [Int]
  }

ix :: SomeVar -> Map SomeVar SymbolicInfo -> Int
ix x m = maybe 0 currentIndex (Map.lookup x m)

inc :: StorageType -> SomeVar -> Int -> Map SomeVar SymbolicInfo -> Map SomeVar SymbolicInfo
inc ty x k m
  | x `elem` Map.keys m = Map.update (\(SymbolicInfo c ns ks) -> Just (SymbolicInfo (c + 1) (updateNegs (c+1) ns) (k:ks))) x m
  | otherwise = Map.insert x (SymbolicInfo 1 (updateNegs 1 []) [k]) m
  where
    updateNegs c = case ty of
      KeepInput -> id
      DropInput -> (c:)

-- indices of symbolic values stored and their chronological position
storedValues :: SymbolicInfo -> [(Int,Int)]
storedValues SymbolicInfo{..} = filter ((`notElem` negativeIndices).fst) $ zip [currentIndex,currentIndex-1..1] chronology

pathDepth :: Path -> Int
pathDepth =  length . fst3 . partitionPath

partitionPath :: Path -> ([Constraint 'Input],[Constraint 'Condition],[Constraint 'Overflow])
partitionPath = foldMap phi where
  phi :: Some Constraint -> ([Constraint 'Input],[Constraint 'Condition],[Constraint 'Overflow])
  phi (Some c@InputConstraint{}) = ([c],mempty,mempty)
  phi (Some c@ConditionConstraint{}) = (mempty,[c],mempty)
  phi (Some c@OverflowConstraints{}) = (mempty,mempty,[c])


showSimplePath :: SimplePath -> String
showSimplePath (ClosedPath cs) = unlines $ intersperse " |" $ map showSomeSimpleConstraint cs
showSimplePath (OpenPath cs c p) =
  (unlines $ intersperse " |" $ map showSomeSimpleConstraint cs)
  ++ (if null cs then "" else " |\n") ++ showSimpleConstraint c ++ "\n |\n"
  ++ showSimplePath p

showSomeSimpleConstraint :: Some SimpleConstraint -> String
showSomeSimpleConstraint (Some c) = showSimpleConstraint c

showSimpleConstraint :: SimpleConstraint t -> String
showSimpleConstraint (SimpleInput KeepInput x vs) = unwords [varname x,":",showValueSet vs, "(kept)"]
showSimpleConstraint (SimpleInput DropInput x vs) = unwords [varname x,":",showValueSet vs, "(dropped)"]
showSimpleConstraint (SimpleCondition c) = showTerm c
showSimpleConstraint (SimpleOverflow _) = "**some overflow checks**"

showPath :: Path -> String
showPath = unlines . intersperse " |" . map showSomeConstraint

showSomeConstraint :: Some Constraint -> String
showSomeConstraint (Some c) = showConstraint c

showConstraint :: Constraint t -> String
showConstraint (InputConstraint (x,i) vs _) = concat [varname x,"_",show i," : ",showValueSet vs]
showConstraint (ConditionConstraint t _) = showTerm t
showConstraint (OverflowConstraints _ _) = "**some overflow checks**"

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
  p i k n = k * (i ^ (n+1) - 1) `div` (i - 1)
  -- path with length (=n) in an iteration with i paths to Nop and k paths to E under the iteration
  p' :: Integer -> Integer -> Integer -> Integer
  p' i k n = k * i ^ n
