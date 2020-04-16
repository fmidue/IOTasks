{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Term.AST (
  AST(..),
  leafs,
  astVars,
  printAST,
  ) where

import Data.Either (rights)

import Data.Term.Class

-- Printable representation of terms
data AST v
  = Node String [AST v]
  | Infix (AST v) String (AST v)
  | Lam [String] (AST v)
  | Literal String
  | UVar (v,Usage) -- variable with usage information
  | SVar v -- simple variable
  deriving (Eq,Show)

leafs :: AST v -> [Either String v]
leafs (Node _ ts) = concatMap leafs ts
leafs (Infix l _ r) = leafs l ++ leafs r
leafs (Lam _ t) = leafs t
leafs (Literal x) = [Left x]
leafs (UVar (x,_)) = [Right x]
leafs (SVar x) = [Right x]

astVars :: AST v -> [v]
astVars = rights . leafs

printAST :: Show v => AST v -> String
printAST = printAST' "" ""
  where
    printAST' o c (Node f xs) = o ++ f ++ " " ++ unwords (fmap (printAST' "(" ")") xs) ++ c
    printAST' o c (Infix x op y) = o ++ printAST x ++ " " ++ op ++ " " ++ printAST y ++ c
    printAST' o c (Lam vs b) = o ++ "\\" ++ unwords vs ++ " -> " ++ printAST b ++ c
    printAST' _ _ (Literal x) = x
    printAST' _ _ (UVar (x,Current)) = show x ++ "_C"
    printAST' _ _ (UVar (x,All)) = show x ++ "_A"
    printAST' _ _ (SVar x) = show x
