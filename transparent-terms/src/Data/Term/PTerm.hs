{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Term.PTerm where

import Data.Environment.Class
import Data.List (nub)
import Data.Proxy

import Type.Reflection (Typeable)

import Data.Term.Class
import Data.Term.AST
import qualified Data.Term.Typed.AST as Typed

-- terms with (partial) pattern matching capabilities
data PTerm v a where
  (:==) :: Eq a => PTerm v a -> PTerm v a -> PTerm v Bool
  (:>) :: Ord a => PTerm v a -> PTerm v a -> PTerm v Bool
  (:+) :: (Num a, Eq a) => PTerm v a -> PTerm v a -> PTerm v a
  (:*) :: (Num a, Fractional a, Eq a) => PTerm v a -> PTerm v a -> PTerm v a
  Mod :: (Integral a, Eq a) => PTerm v a -> PTerm v a -> PTerm v a
  (:&&) :: PTerm v Bool -> PTerm v Bool -> PTerm v Bool
  Not :: PTerm v Bool -> PTerm v Bool
  Length :: Eq a => PTerm v [a] -> PTerm v Int
  Init :: PTerm v [a] -> PTerm v [a]
  Last :: PTerm v [a] -> PTerm v a
  Reverse :: PTerm v [a] -> PTerm v [a]
  Sum :: Num a => PTerm v [a] -> PTerm v a
  Product :: Num a => PTerm v [a] -> PTerm v a
  Filter :: Num a => (a -> Bool) -> PTerm v [a] -> PTerm v [a]
  Lit :: Show a => a -> PTerm v a
  GetCurrent :: Typeable a => v -> PTerm v a
  GetAll :: Typeable a => v -> PTerm v [a]

instance VarTerm (PTerm v) v where
  variable' = GetCurrent

instance PVarTerm (PTerm v) v where
  variableAll' = GetAll

instance Eq v => VarListTerm (PTerm v) v where
  termVars (x :== y) = nub $ termVars x ++ termVars y
  termVars (x :> y) = nub $ termVars x ++ termVars y
  termVars (x :+ y) = nub $ termVars x ++ termVars y
  termVars (x :* y) = nub $ termVars x ++ termVars y
  termVars (Mod x y) = nub $ termVars x ++ termVars y
  termVars (x :&& y) = nub $ termVars x ++ termVars y
  termVars (Not x) = termVars x
  termVars (Length x) = termVars x
  termVars (Init x) = termVars x
  termVars (Last x) = termVars x
  termVars (Reverse x) = termVars x
  termVars (Sum x) = termVars x
  termVars (Product x) = termVars x
  termVars (Filter _ xs) = termVars xs
  termVars (Lit _) = []
  termVars (GetCurrent x) = [x]
  termVars (GetAll x) = [x]

instance UsageTerm (PTerm v) v where
  varUsage = error "UsageTerm (PTerm v) v: not implemented"

instance SynTerm (PTerm v) (AST v) where
   viewTerm (x :== y) = infixApp (viewTerm x) "==" (viewTerm y)
   viewTerm (x :> y) = infixApp (viewTerm x) ">" (viewTerm y)
   viewTerm (x :+ y) = infixApp (viewTerm x) "+" (viewTerm y)
   viewTerm (x :* y) = infixApp (viewTerm x) "*" (viewTerm y)
   viewTerm (Mod x y) = infixApp (viewTerm x) "`mod`" (viewTerm y)
   viewTerm (x :&& y) = infixApp (viewTerm x) "&&" (viewTerm y)
   viewTerm (Not x) = Leaf "not" `app` viewTerm x
   viewTerm (Length x) = Leaf "length" `app` viewTerm x
   viewTerm (Init x) = Leaf "init" `app` viewTerm x
   viewTerm (Last x) = Leaf "last" `app` viewTerm x
   viewTerm (Reverse x) = Leaf "reverse" `app` viewTerm x
   viewTerm (Sum x) = Leaf "sum" `app` viewTerm x
   viewTerm (Product x) = Leaf "product" `app` viewTerm x
   viewTerm (Filter _ xs) = Leaf "filter ???" `app` viewTerm xs
   viewTerm (Lit x) = Leaf $ show x
   viewTerm (GetCurrent x) = Var x
   viewTerm (GetAll x) = VarA x

instance SynTermTyped (PTerm v) (Typed.AST v) where
  viewTermTyped = error "SynTermTyped (PTerm v) (Typed.AST v): not implemented"

instance (PVarEnv env v, Show v) => SemTerm (PTerm v) (env v)  where
  evalTerm (x :== y) env = evalTerm x env == evalTerm y env
  evalTerm (x :> y) env = evalTerm x env > evalTerm y env
  evalTerm (x :+ y) env = evalTerm x env + evalTerm y env
  evalTerm (x :* y) env = evalTerm x env * evalTerm y env
  evalTerm (Mod x y) env = evalTerm x env `mod` evalTerm y env
  evalTerm (x :&& y) env = evalTerm x env && evalTerm y env
  evalTerm (Not x) env = not $ evalTerm x env
  evalTerm (Length x) env = length $ evalTerm x env
  evalTerm (Init x) env = init $ evalTerm x env
  evalTerm (Last x) env = last $ evalTerm x env
  evalTerm (Reverse x) env = reverse $ evalTerm x env
  evalTerm (Sum x) env = sum $ evalTerm x env
  evalTerm (Product x) env = product $ evalTerm x env
  evalTerm (Filter f xs) env = filter f $ evalTerm xs env
  evalTerm (Lit x) _ = x
  evalTerm (GetCurrent x) env =
    case lookupAtType Proxy x env of
      Left e -> error $ printLookupError e
      Right vs -> vs
  evalTerm (GetAll x) env =
    case lookupAllAtType Proxy x env of
      Left e -> error $ printLookupError e
      Right vs -> vs

instance ClosedSemTerm (PTerm v) where
  evalClosed (x :== y) = evalClosed x == evalClosed y
  evalClosed (x :> y) = evalClosed x > evalClosed y
  evalClosed (x :+ y) = evalClosed x + evalClosed y
  evalClosed (x :* y) = evalClosed x * evalClosed y
  evalClosed (Mod x y) = evalClosed x `mod` evalClosed y
  evalClosed (x :&& y) = evalClosed x && evalClosed y
  evalClosed (Not x) = not $ evalClosed x
  evalClosed (Length x) = length $ evalClosed x
  evalClosed (Init x) = init $ evalClosed x
  evalClosed (Last x) = last $ evalClosed x
  evalClosed (Reverse x) = reverse $ evalClosed x
  evalClosed (Sum x) = sum $ evalClosed x
  evalClosed (Product x) = product $ evalClosed x
  evalClosed (Filter f xs) = filter f $ evalClosed xs
  evalClosed (Lit x) = x
  evalClosed (GetCurrent _) = error "evalClosed: term is not closed"
  evalClosed (GetAll _) = error "evalClosed: term is not closed"
