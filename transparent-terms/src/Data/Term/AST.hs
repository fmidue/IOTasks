{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Term.AST  where

import Data.Tree
import Data.Tree.Pretty

import Data.Term.Class

-- Printable representation of terms
data AST v
  = Leaf String
  | Lam Int (String -> AST v)
  | Var v
  | VarA v
  | App (AST v) (AST v)
  | PostApp (AST v) (AST v)

printTree :: Show v => AST v -> Tree String
printTree (App f x) = Node "($)" [printTree f, printTree x]
printTree (PostApp x f) = Node "(&)" [printTree f, printTree x]
printTree (Leaf s) = Node s []
printTree (Lam x t) = Node ("\\" ++ show x ++ " -> ") [printTree (t $ "var " ++ show x)]
printTree (Var x) = Node (show x) []
printTree (VarA x) = Node (show x ++ "_A") []

pPrintTree :: Show v => AST v -> String
pPrintTree = drawVerticalTree . printTree

printFlatS :: (v -> String) -> AST v -> String
printFlatS _ (Leaf s) = s
printFlatS sho (Lam x t) = "(\\var" ++ show x ++ " -> " ++ printFlatS sho (t $ "var" ++ show x) ++ ")"
printFlatS sho (Var x) = sho x
printFlatS sho (VarA x) = sho x ++ "_A"
printFlatS sho (App f (Leaf s)) = printFlatS sho f ++ " " ++ s
printFlatS sho (App f (Var x)) = printFlatS sho f ++ " " ++ sho x
printFlatS sho (App f x) = printFlatS sho f ++ " (" ++ printFlatS sho x ++ ")"
printFlatS sho (PostApp (Leaf s) f) = s ++ " " ++ printFlatS sho f
printFlatS sho (PostApp (Var x) f) = sho x ++ " " ++ printFlatS sho f
printFlatS sho (PostApp x f) = "(" ++ printFlatS sho x ++ ") " ++ printFlatS sho f

printFlat :: Show v => AST v -> String
printFlat = printFlatS show

printFlat' :: AST String -> String
printFlat' = printFlatS id

printFlatClosed :: AST v -> String
printFlatClosed = printFlatS undefined

vars :: AST v -> [v]
vars (Leaf _) = []
vars (Lam _ t) = vars $ t undefined -- local variables are not considered here
vars (Var x) = [x]
vars (VarA x) = [x]
vars (App f x) = vars f ++ vars x
vars (PostApp x f) = vars x ++ vars f

mapV :: (v -> v') -> AST v -> AST v'
mapV _ (Leaf s) = Leaf s
mapV f (Lam x t) = Lam x $ \x -> mapV f (t x)
mapV f (Var x) = Var (f x)
mapV f (VarA x) = VarA (f x)
mapV f (App g x) = App (mapV f g) (mapV f x)
mapV f (PostApp x g) = PostApp (mapV f x) (mapV f g)

replaceVar :: (v -> Usage -> AST v') -> AST v -> AST v'
replaceVar _ (Leaf s) = Leaf s
replaceVar f (Lam x t) = Lam x $ \x -> replaceVar f (t x)
replaceVar f (Var x) = f x Current
replaceVar f (VarA x) = f x All
replaceVar f (App g x) = App (replaceVar f g) (replaceVar f x)
replaceVar f (PostApp x g) = PostApp (replaceVar f x) (replaceVar f g)
