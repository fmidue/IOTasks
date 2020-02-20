{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTasks.Term (
  Term(..),
  getCurrent,
  getAll,
  TermVars(..),
  SynTerm(..),
  SemTerm(..),
  AST(..),
  printAST,
  printTerm,
) where

import Data.Dynamic (Typeable)

import Data.Kind

import Test.IOTasks.Environment (Varname, Environment)

class Term (t :: * -> *) where
  getCurrent' :: Typeable a => Varname -> t a
  getAll' :: Typeable a => Varname -> t [a]

-- convenience reordering of quantification
getCurrent :: forall a t. (Term t, Typeable a) => Varname -> t a
getCurrent = getCurrent'

getAll :: forall a t. (Term t, Typeable a) => Varname -> t [a]
getAll = getAll'

type family F x :: Constraint where
  F x = ()

class TermVars (t :: * -> *) where
  termVars :: t a -> [Varname]

-- syntactic terms
class SynTerm (t :: * -> *) where
  viewTerm :: t a -> AST

-- semantic terms
class SemTerm (t :: * -> *) where
  evalTerm :: t a -> Environment -> a

-- Printable representation of terms
data AST
  = Node String [AST]
  | Infix AST String AST
  | Lam [Varname] AST
  | Leaf String
  deriving Show

printAST :: AST -> String
printAST = printAST' "" ""
  where
    printAST' o c (Node f xs) = o ++ f ++ " " ++ unwords (fmap (printAST' "(" ")") xs) ++ c
    printAST' o c (Infix x op y) = o ++ printAST x ++ " " ++ op ++ " " ++ printAST y ++ c
    printAST' o c (Lam vs b) = o ++ "\\" ++ unwords vs ++ " -> " ++ printAST b ++ c
    printAST' _ _ (Leaf x) = x

printTerm :: SynTerm t => t a -> String
printTerm = printAST . viewTerm
