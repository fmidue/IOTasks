{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Term.AST (
  AST(..),
  Varname,
  leafs,
  astVars,
  printAST,
  printTerm,
  ) where

import Data.Either (rights)

import Data.Term.Class

type Varname = String

-- Printable representation of terms
data AST v
  = Node String [AST v]
  | Infix (AST v) String (AST v)
  | Lam [Varname] (AST v)
  | Literal String
  | Var (v,Usage)
  deriving (Eq,Show)

leafs :: AST v -> [Either String v]
leafs (Node _ ts) = concatMap leafs ts
leafs (Infix l _ r) = leafs l ++ leafs r
leafs (Lam _ t) = leafs t
leafs (Literal x) = [Left x]
leafs (Var (x,_)) = [Right x]

astVars :: AST v -> [v]
astVars = rights . leafs

printAST :: Show v => AST v -> String
printAST = printAST' "" ""
  where
    printAST' o c (Node f xs) = o ++ f ++ " " ++ unwords (fmap (printAST' "(" ")") xs) ++ c
    printAST' o c (Infix x op y) = o ++ printAST x ++ " " ++ op ++ " " ++ printAST y ++ c
    printAST' o c (Lam vs b) = o ++ "\\" ++ unwords vs ++ " -> " ++ printAST b ++ c
    printAST' _ _ (Literal x) = x
    printAST' _ _ (Var (x,Current)) = show x ++ "_C"
    printAST' _ _ (Var (x,All)) = show x ++ "_A"

printTerm :: (forall v. SynTerm t (AST v),forall v. Show v) => t a -> String
printTerm = printAST . viewTerm
