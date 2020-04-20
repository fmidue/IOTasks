{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Data.Term.Typed.AST where

import Type.Reflection (Typeable , TypeRep, typeRep)
import Data.Proxy
import Data.Environment

import Data.Term.Liftable
import Data.Term.Class

import Data.Tree
import Data.Tree.Pretty

data AST :: * -> * -> * where
  Leaf :: a -> String -> AST v a
  Lam :: Int -> (AST v x -> AST v a) -> AST v (x -> a)
  Var :: Typeable a => v -> TypeRep a -> (a -> String) -> AST v a
  App :: AST v (a -> b) -> AST v a -> AST v b

instance Liftable (AST v) where
  appT = App
  embedT (x,s) = Leaf x s

  -- TODO: improve this
  liftTInfix = liftT2

  unHO = Lam 0
  unHO2 f = Lam 1 $ \x -> Lam 0 $ f x
  unHO3 f = Lam 2 $ \x -> Lam 1 $ \y -> Lam 0 $ f x y

instance VarTerm (AST v) v where
  variable' v = Var v typeRep

instance VarListTerm (AST v) v where
  termVars = vars

instance Show v => SynTerm (AST v) (Tree String) where
  viewTerm = printTree

instance SynTermTyped (AST v) (AST v) where
  viewTermTyped = id

instance (VarEnv env v, Ord v, Show  v) => SemTerm (AST v) (env v) where
  evalTerm = flip eval

reduceAp :: AST v a -> AST v a
reduceAp (App (Lam _ t) (Leaf y sy)) = t (Leaf y sy)
reduceAp x = x

reduceKnownVariable :: (VarEnv env v, Ord v) => AST v a -> env v -> AST v a
reduceKnownVariable var@(Var x _ sho) e =
  case lookupAtType Proxy x e of
    Left _ -> var
    Right v -> Leaf v (sho v)
reduceKnownVariable x _ = x

reduceT :: (VarEnv env v, Ord v) => env v -> AST v a -> AST v a
reduceT e (App x y) = reduceAp $ App (reduceT e x) (reduceT e y)
reduceT _ (Leaf x s) = Leaf x s
reduceT e (Lam x t) = Lam x (reduceT e . t)
reduceT e var@Var{} = reduceKnownVariable var e

printTree :: Show v => AST v a -> Tree String
printTree (App tx ty) = Node "($)" [printTree tx, printTree ty]
printTree (Leaf _ s) = Node s []
printTree (Lam x t) = Node ("\\" ++ show x ++ " -> ") [printTree (t (Leaf undefined ("var " ++ show x)))]
printTree (Var x ty _) = Node (show x ++ ":" ++ show ty) []

pPrintTree :: Show v => AST v a -> String
pPrintTree = drawVerticalTree . printTree

eval :: (VarEnv env v, Ord v, Show v) => env v -> AST v a -> a
eval e (App tx ty) = eval e tx $ eval e ty
eval _ (Leaf x _) = x
eval e (Lam _ t) = \x -> eval e (t (Leaf x undefined))
eval e (Var x _ _) =
  case lookupAtType Proxy x e of
    Left err -> error $ printLookupError err
    Right v -> v

vars :: AST v a -> [v]
vars (Leaf _ _) = []
vars (Lam _ t) = vars $ t (Leaf undefined undefined) -- local variables are not considered here
vars (Var x _ _) = [x]
vars (App f x) = vars f ++ vars x
