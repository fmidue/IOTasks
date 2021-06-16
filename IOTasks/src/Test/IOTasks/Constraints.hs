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
import Data.Holmes

data ConstraintTree
  = Assert Constraint ConstraintTree
  | Split ConstraintTree ConstraintTree
  | Leaf

data Constraint where
  TypeConstaint :: (Varname,Int) -> SomeTypeRep -> Constraint
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
  let e' u = if u == x then e u + 1 else e u
  in Assert (TypeConstaint (x,e x) $ typeOfValueSet vs) $ constraintTree' s' k e'
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

pathBaseVars :: Path -> [Varname]
pathBaseVars = nub . concatMap extractVar
  where
    extractVar (TypeConstaint (v,_) _) = [v]
    extractVar (PredicateConstraint c _) = termVars c

printConstraint :: Constraint -> String
printConstraint (TypeConstaint (x,i) ty) = x ++ show i ++ " : " ++ show ty
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

holmesConstraint :: Path -> (Int,HolmesConstraint)
holmesConstraint path = (nVars, constr)
  where
    nVars = length tyConstr
    constr = HolmesConstraint $ \vs -> and' [ let f = constrH c (ixE e) in f vs | ~(PredicateConstraint c e) <- predConstr]
    (tyConstr,predConstr) = partition (\case {TypeConstaint{} -> True; _ -> False}) path
    allVars = (\(TypeConstaint (v,_) _) -> v) <$> tyConstr
    -- maps Varnames to the indices at which the respective constraint variables occur
    ix x = [ i | (y,i) <- allVars `zip` [0..], x == y]
    -- same as ix but respects the environment of a constraint, i.e., the fact that not all inputs for a varaible are "known" for a constraint
    ixE e x = take (e x -1) $ ix x
    -- all variable names that are read into on the path (with multiples and in order of reading)

constrH :: Eq a => MonadCell m => PTerm Varname a -> (Varname -> [Int]) -> [Prop m (Defined Int)] -> Prop m (Defined a)
constrH (x :== y) ix vs = constrH x ix vs .== constrH y ix vs
constrH (x :> y) ix vs = constrH x ix vs .> constrH y ix vs
constrH (x :+ y) ix vs = constrH x ix vs .+ constrH y ix vs
constrH (x :* y) ix vs = constrH x ix vs .* constrH y ix vs
constrH (Mod x y) ix vs = constrH x ix vs .%. constrH y ix vs
constrH (x :&& y) ix vs = constrH x ix vs .&& constrH y ix vs
constrH (Not x) ix vs = not' $ constrH x ix vs
constrH (Length x) ix _ = lift $ lengthT x ix
constrH (Init x) ix vs = init .$ constrH x ix vs
constrH (Last x) ix vs = last .$ constrH x ix vs
constrH (Reverse x) ix vs = reverse .$ constrH x ix vs
constrH (Sum x) ix vs = sum .$ constrH x ix vs
constrH (Product x) ix vs = product .$ constrH x ix vs
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

lengthT :: PTerm Varname [a] -> (Varname -> [Int]) -> Int
lengthT (Init x) ix = lengthT x ix -1
lengthT (Reverse x) ix = lengthT x ix
lengthT (Filter _ _) _ = error "length of filter unsupported"
lengthT (Lit x) _ = length x
lengthT (GetAll x :: PTerm Varname [a]) ix = length $ ix x
lengthT _ _ = error "unsupported lengthT"
