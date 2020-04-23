{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  Lam :: Int -> ((x,String) -> AST v a) -> AST v (x -> a)
  Var :: Typeable a => v -> TypeRep a -> AST v a
  VarA :: Typeable a => v -> TypeRep a -> AST v [a]
  App :: AST v (a -> b) -> AST v a -> AST v b

instance Liftable (AST v) where
  appT = App
  embedT (x,s) = Leaf x s

  -- TODO: improve this
  liftTInfix = liftT2

  unHO f = Lam 0 $ \(x,s) -> f $ Leaf x s
  unHO2 f = Lam 1 $ \(x,sx) -> Lam 0 $ \(y,sy) -> f (Leaf x sx) (Leaf y sy)
  unHO3 f = Lam 2 $ \(x,sx) -> Lam 1 $ \(y,sy) -> Lam 0 $ \(z,sz) -> f (Leaf x sx) (Leaf y sy) (Leaf z sz)

instance VarTerm (AST v) v where
  variable' v = Var v typeRep

instance PVarTerm (AST v) v where
  variableAll' v = VarA v typeRep

instance VarListTerm (AST v) v where
  termVars = vars

instance Show v => SynTerm (AST v) (Tree String) where
  viewTerm = printTree

instance SynTermTyped (AST v) (AST v) where
  viewTermTyped = id

instance (PVarEnv env v, Ord v, Show  v) => SemTerm (AST v) (env v) where
  evalTerm = flip eval

reduceAp :: AST v a -> AST v a
reduceAp (App (Lam _ t) (Leaf y sy)) = t (y,sy)
reduceAp x = x

reduceKnownVariable :: (VarEnv env v, Ord v) => AST v a -> env v -> AST v a
reduceKnownVariable var@(Var x _) e =
  case lookupAtTypeWithShow Proxy x e of
    Left _ -> var
    Right (v,sho) -> Leaf v (sho v)
reduceKnownVariable x _ = x

reduceT :: (VarEnv env v, Ord v) => env v -> AST v a -> AST v a
reduceT e (App x y) = reduceAp $ App (reduceT e x) (reduceT e y)
reduceT _ (Leaf x s) = Leaf x s
reduceT e (Lam x t) = Lam x (reduceT e . t)
reduceT e var@Var{} = reduceKnownVariable var e
reduceT e var@VarA{} = _ -- reduceKnownVariable var e

tryValue :: AST v a ->  Maybe a
tryValue (Leaf x _) = Just x
tryValue _ = Nothing

printTree :: Show v => AST v a -> Tree String
printTree (App tx ty) = Node "($)" [printTree tx, printTree ty]
printTree (Leaf _ s) = Node s []
printTree (Lam x t) = Node ("\\" ++ show x ++ " -> ") [printTree (t (undefined,"var " ++ show x))]
printTree (Var x ty) = Node (show x ++ ":" ++ show ty) []
printTree (VarA x ty) = Node (show x ++ "_A" ++ ":" ++ show ty) []

pPrintTree :: Show v => AST v a -> String
pPrintTree = drawVerticalTree . printTree

printFlat :: Show v => AST v a -> String
printFlat (Leaf _ s) = s
printFlat (Lam x t) = "\\" ++ show x ++ " -> " ++ printFlat (t (undefined, "var " ++ show x))
printFlat (Var x r) = show x ++ ":" ++ show r
printFlat (VarA x r) = show x ++ "_A" ++ ":" ++ show r
printFlat (App f x) = "(" ++ printFlat f ++ ")(" ++ printFlat x ++ ")"

eval :: (PVarEnv env v, Ord v, Show v) => env v -> AST v a -> a
eval e (App tx ty) = eval e tx $ eval e ty
eval _ (Leaf x _) = x
eval e (Lam _ t) = \x -> eval e (t (x, undefined))
eval e (Var x _) =
  case lookupAtType Proxy x e of
    Left err -> error $ printLookupError err
    Right v -> v
eval e (VarA x _) =
  case lookupAllAtType Proxy x e of
    Left err -> error $ printLookupError err
    Right v -> v

vars :: AST v a -> [v]
vars (Leaf _ _) = []
vars (Lam _ t) = vars $ t (undefined, undefined) -- local variables are not considered here
vars (Var x _ ) = [x]
vars (VarA x _ ) = [x]
vars (App f x) = vars f ++ vars x

mapV :: (v -> v') -> AST v a -> AST v' a
mapV _ (Leaf x s) = Leaf x s
mapV f (Lam x t) = Lam x $ \x -> mapV f (t x)
mapV f (Var x r) = Var (f x) r
mapV f (VarA x r) = VarA (f x) r
mapV f (App g x) = App (mapV f g) (mapV f x)

replaceVar :: (forall a. Typeable a => v -> TypeRep a -> AST v' a) -> AST v a -> AST v' a
replaceVar _ (Leaf x s) = Leaf x s
replaceVar f (Lam x t) = Lam x $ \x -> replaceVar f (t x)
replaceVar f (Var x r) = f x r
replaceVar f (App g x) = App (replaceVar f g) (replaceVar f x)
