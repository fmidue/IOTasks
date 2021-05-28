{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Constraints where

import Data.Void
import Data.Environment
import Data.Term.PTerm
import qualified Data.Term.AST as AST
import Data.Term.AST (AST)
import Data.Term.Class
import Test.IOTasks.Specification
import Test.IOTasks.ValueSet

import Type.Reflection
import Data.List
import Data.Maybe
import Data.Holmes
import Data.Char (isDigit)
import qualified Data.Vector as Vec
import Data.Vector ((!))


data ConstraintTree
  = Assert Constraint ConstraintTree
  | Split ConstraintTree ConstraintTree
  | Leaf

data Constraint where
  TypeConstaint :: Varname -> SomeTypeRep -> Constraint
  PredicateConstraint :: PTerm Varname Bool -> Env -> Constraint

instance Eq Constraint where
  (TypeConstaint x tx) == (TypeConstaint y ty) = x == y && tx == ty
  (PredicateConstraint tx ex) == (PredicateConstraint ty ey) = viewTerm' tx ex == viewTerm' ty ey
  _ == _ = False

type Path = [Constraint]

typeOfValueSet :: ValueSet -> SomeTypeRep
typeOfValueSet vs = withProxy vs someTypeRep

constraintTree :: Specification (PTerm Varname) -> ConstraintTree
constraintTree (Spec s) = constraintTree' s kI (const 1)
  where kI Continue _ = Leaf
        kI Break    _ = error "toplevel break"

data Step = Continue | Break

type VarCounter = Int
type Env = Varname -> VarCounter

constraintTree' :: [Action (Specification (PTerm Varname)) (PTerm Varname)] -> (Step -> Env -> ConstraintTree) -> Env -> ConstraintTree
constraintTree' (ReadInput x vs : s') k e =
  let v = x ++ show (e x)
      e' u = if u == x then e u + 1 else e u
  in Assert (TypeConstaint v $ typeOfValueSet vs) $ constraintTree' s' k e'
constraintTree' (WriteOutput{} : s') k e = constraintTree' s' k e
constraintTree' (TillE (Spec s) : s') k e =
  let k' Continue = constraintTree' s  k'
      k' Break    = constraintTree' s' k
  in constraintTree' s k' e
constraintTree' (Branch c (Spec s11) (Spec s12) : s2) k e =
  Split
    (Assert (PredicateConstraint (Not c) e) $ constraintTree' (s11 ++ s2) k e)
    (Assert (PredicateConstraint c e) $ constraintTree' (s12 ++ s2) k e)
constraintTree' (E : _) k e = k Break e
constraintTree' [] k e = k Continue e

extendWith :: ConstraintTree -> ConstraintTree -> ConstraintTree
extendWith (Assert c t') y = Assert c (extendWith t' y)
extendWith (Split t1 t2) y = Split (extendWith t1 y) (extendWith t2 y)
extendWith Leaf y = y

paths :: ConstraintTree -> [Path]
paths t = go 1 where
  go n = pathsWithNInputs n t ++ go (n + 1)

pathsWithNInputs :: Int -> ConstraintTree -> [Path]
pathsWithNInputs 0 Leaf          = [[]]
pathsWithNInputs 0 _             = []
pathsWithNInputs _ Leaf          = []
pathsWithNInputs n (Assert c t') = (c :) <$> pathsWithNInputs (n-1) t'
pathsWithNInputs n (Split t1 t2) = pathsWithNInputs n t1 ++ pathsWithNInputs n t2

depth :: [Constraint] -> Int
depth = length . filter (\case { PredicateConstraint{} -> True; _ -> False })

pathsWithMaxDepth :: Int -> ConstraintTree -> [Path]
pathsWithMaxDepth d = takeWhile ((<= d). depth) . paths

printConstraint :: Constraint -> String
printConstraint (TypeConstaint x ty) = x ++ " : " ++ show ty
printConstraint (PredicateConstraint c e) = viewTerm' c e

fillInVariables :: AST Varname -> Env -> AST Void
fillInVariables t e = AST.replaceVar f t
  where
    f x All = AST.Leaf $ "[" ++  intercalate ", " [ x ++ show n | n <- [1 .. e x - 1]] ++ "]"
    f x Current = AST.Leaf $ x ++ show (e x - 1)

viewTerm' :: PTerm Varname a -> Env -> String
viewTerm' t e = AST.printFlatClosed $ fillInVariables (viewTerm t) e

printConstraints :: [Constraint] -> String
printConstraints = intercalate " /\\ " . fmap printConstraint

printPaths :: [[Constraint]] -> String
printPaths = unlines . fmap printConstraints

printTree :: Int -> ConstraintTree -> String
printTree _ Leaf          = "Leaf"
printTree n _ | n <= 0    = "..."
printTree n (Assert c t') = concat ["Assert (",printConstraint c, ") (" , printTree (n-1) t' , ")"]
printTree n (Split t1 t2) = concat ["Split (",printTree (n-1) t1, ") (" , printTree (n-1) t2 , ")"]

data PathCrumbs = L | R

printBranch :: [PathCrumbs] -> ConstraintTree -> String
printBranch _      Leaf          = "Leaf"
printBranch xs     (Assert c t') = concat ["Assert (",printConstraint c, ") ", printBranch xs t']
printBranch []     _             = "Split ..."
printBranch (L:xs) (Split t1 _ ) = "SplitL " ++ printBranch xs t1
printBranch (R:xs) (Split _  t2) = "SplitR " ++ printBranch xs t2

-- holmes constraint
newtype HolmesConstraint = HolmesConstraint (forall m. MonadCell m => [Prop m (Defined Int)] -> Prop m (Defined Bool))

holmesConstraint :: Path -> (Int,[a] -> [a],HolmesConstraint)
holmesConstraint path = (nVars, arrangeVars, constr)
  where
    constr = HolmesConstraint $ \vs -> and' [ let f = constrH c (ixVE e) in f vs | ~(PredicateConstraint c e) <- predConstr]
    ixVE e x = take (e x -1) $ ixV x
    ixV x = fromMaybe [] $ lookup x ix
    (tyConstr,predConstr) = partition (\case {TypeConstaint{} -> True; _ -> False}) path
    nVars = length tyConstr
    maxIx :: Varname -> Int
    maxIx x = maximum [ e x -1 | ~(PredicateConstraint _ e) <- predConstr]
    allVars = nub $ concat [ termVars c | ~(PredicateConstraint c _) <- predConstr]
    ixF :: [Varname] -> Int -> [(Varname,[Int])]
    ixF [] _ = []
    ixF (x:xs) i = let mX = maxIx x in (x,[i..i + mX]) : ixF xs (i + mX)
    ix =  ixF allVars 0
    -- rearanging the variables to match the input sequence
    arrangeVars xs =
      let
        old = Vec.fromList xs
        pos = Vec.fromList $ [ ixV x !! (read n - 1) | (TypeConstaint xN _) <- tyConstr, let (x,n) = splitVarName xN ]
      in Vec.toList $ Vec.generate nVars (\i -> old ! (pos ! i))
    splitVarName = break isDigit

constrH :: Eq a => MonadCell m => PTerm Varname a -> (Varname -> [Int]) -> [Prop m (Defined Int)] -> Prop m (Defined a)
constrH (x :== y) ix vs = constrH x ix vs .== constrH y ix vs
constrH (x :> y) ix vs = constrH x ix vs .> constrH y ix vs
constrH (x :+ y) ix vs = constrH x ix vs .+ constrH y ix vs
constrH (Mod x y) ix vs = constrH x ix vs .%. constrH y ix vs
constrH (x :&& y) ix vs = constrH x ix vs .&& constrH y ix vs
constrH (Not x) ix vs = not' $ constrH x ix vs
constrH (Length x) ix vs = length .$ constrH x ix vs
constrH (Init x) ix vs = init .$ constrH x ix vs
constrH (Last x) ix vs = last .$ constrH x ix vs
constrH (Reverse x) ix vs = reverse .$ constrH x ix vs
constrH (Sum x) ix vs = sum .$ constrH x ix vs
constrH (Filter f xs) ix vs = filter f .$ constrH xs ix vs
constrH (Lit x) _ _ = lift x
constrH (GetAll x :: PTerm Varname a) ix vs =
  case eqTypeRep (typeRep @a) (typeRep @[Int]) of
    Just HRefl -> foldr (zipWith' (:)) (lift []) $ [ vs !! i | i <- ix x ]
    Nothing -> error "implement non-Int types!"
constrH (GetCurrent x :: PTerm Varname a) ix vs =
  case eqTypeRep (typeRep @a) (typeRep @Int) of
    Just HRefl -> vs !! last (ix x)
    Nothing -> error "implement non-Int types!"
