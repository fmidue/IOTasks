{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Term.Typed.AST where

import Type.Reflection (Typeable , TypeRep, typeRep, eqTypeRep, (:~~:)(HRefl))
import Data.Proxy
import Data.Environment

import Data.Term.Liftable
import Data.Term.Class
import qualified Data.Term.AST as Untyped

import Data.Tree
import Data.Tree.Pretty

data AST :: * -> * -> * where
  Leaf :: a -> String -> AST v a
  Lam :: Int -> ((x,String) -> AST v a) -> AST v (x -> a)
  Var :: Typeable a => v -> TypeRep a -> AST v a
  VarA :: Typeable a => v -> TypeRep [a] -> AST v [a]
  App :: AST v (a -> b) -> AST v a -> AST v b -- regular prefix application
  PostApp :: AST v a -> AST v (a -> b) -> AST v b -- postfix application

instance Liftable (AST v) where
  appT = App
  embedT (x,s) = Leaf x s

  liftTInfix f = appT . flip PostApp (embedT f)

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

instance Show v => SynTerm (AST v) (Untyped.AST v) where
  viewTerm = dropTypes

dropTypes :: AST v a -> Untyped.AST v
dropTypes (Leaf _ s) = Untyped.Leaf s
dropTypes (Lam x t) = Untyped.Lam x (\x -> dropTypes $ t (undefined, x))
dropTypes (Var x _) = Untyped.Var x
dropTypes (VarA x _) = Untyped.VarA x
dropTypes (App f x) = Untyped.App (dropTypes f) (dropTypes x)
dropTypes (PostApp x f) = Untyped.PostApp (dropTypes x) (dropTypes f)

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
reduceT e (App f x) = reduceAp $ App (reduceT e f) (reduceT e x)
reduceT e (PostApp x f) = reduceAp $ PostApp (reduceT e x) (reduceT e f)
reduceT _ (Leaf x s) = Leaf x s
reduceT e (Lam x t) = Lam x (reduceT e . t)
reduceT e var@Var{} = reduceKnownVariable var e
reduceT _ VarA{} = error "TODO: implement" -- reduceKnownVariable var e

tryValue :: AST v a ->  Maybe a
tryValue (Leaf x _) = Just x
tryValue (App f x) = tryValue f <*> tryValue x
tryValue (PostApp x f) = tryValue f <*> tryValue x
tryValue (Lam _ _) = Nothing -- not easily possible due to PHOAS encoding (easier if we encode at the type level that a AST is closed )
tryValue Var{} = Nothing
tryValue VarA{} = Nothing

printTree :: Show v => AST v a -> Tree String
printTree (App f x) = Node "($)" [printTree f, printTree x]
printTree (PostApp x f) = Node "(&)" [printTree f, printTree x]
printTree (Leaf _ s) = Node s []
printTree (Lam x t) = Node ("\\" ++ show x ++ " -> ") [printTree (t (undefined,"var " ++ show x))]
printTree (Var x ty) = Node (show x ++ ":" ++ show ty) []
printTree (VarA x ty) = Node (show x ++ "_A" ++ ":" ++ show ty) []

pPrintTree :: Show v => AST v a -> String
pPrintTree = drawVerticalTree . printTree

printFlatS :: (v -> String) -> AST v a -> String
printFlatS _ (Leaf _ s) = s
printFlatS sho (Lam x t) = "(\\var" ++ show x ++ " -> " ++ printFlatS sho (t (undefined, "var" ++ show x)) ++ ")"
printFlatS sho (Var x _) = sho x
printFlatS sho (VarA x _) = sho x ++ "_A"
printFlatS sho (App f (Leaf _ s)) = printFlatS sho f ++ " " ++ s
printFlatS sho (App f (Var x _)) = printFlatS sho f ++ " " ++ sho x
printFlatS sho (App f x) = printFlatS sho f ++ " (" ++ printFlatS sho x ++ ")"
printFlatS sho (PostApp (Leaf _ s) f) = s ++ " " ++ printFlatS sho f
printFlatS sho (PostApp (Var x _) f) = sho x ++ " " ++ printFlatS sho f
printFlatS sho (PostApp x f) = "(" ++ printFlatS sho x ++ ") " ++ printFlatS sho f

printFlat :: Show v => AST v a -> String
printFlat = printFlatS show

printFlat' :: AST String a -> String
printFlat' = printFlatS id

printFlatClosed :: AST v a -> String
printFlatClosed = printFlatS undefined

eval :: (PVarEnv env v, Ord v, Show v) => env v -> AST v a -> a
eval e (App f x) = eval e f $ eval e x
eval e (PostApp x f) = eval e f $ eval e x
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
vars (PostApp x f) = vars x ++ vars f

mapV :: (v -> v') -> AST v a -> AST v' a
mapV _ (Leaf x s) = Leaf x s
mapV f (Lam x t) = Lam x $ \x -> mapV f (t x)
mapV f (Var x r) = Var (f x) r
mapV f (VarA x r) = VarA (f x) r
mapV f (App g x) = App (mapV f g) (mapV f x)
mapV f (PostApp x g) = PostApp (mapV f x) (mapV f g)

replaceVar :: (forall a. Typeable a => v -> Usage -> TypeRep a -> AST v' a) -> AST v a -> AST v' a
replaceVar _ (Leaf x s) = Leaf x s
replaceVar f (Lam x t) = Lam x $ \x -> replaceVar f (t x)
replaceVar f (Var x r) = f x Current r
replaceVar f (VarA x r) = f x All r
replaceVar f (App g x) = App (replaceVar f g) (replaceVar f x)
replaceVar f (PostApp x g) = PostApp (replaceVar f x) (replaceVar f g)

coerseAST :: (Typeable a, Typeable b) => AST v a -> Proxy b -> AST v b
coerseAST (x :: AST v a) (Proxy :: Proxy b) =
  case typeRep @a `eqTypeRep` typeRep @b of
    Just HRefl -> x
    Nothing -> error $ "coersion failed from " ++ show (typeRep @a) ++ " to " ++ show (typeRep @b)
