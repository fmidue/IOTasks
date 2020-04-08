{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTasks.Term (
  Term(..),
  getCurrent,
  getAll,
  var,
  UsageVar(..),
  TermVars(..),
  SynTerm(..),
  Usage(..),
  SpecVar,
  SemTerm(..),
  AST(..),
  printAST,
  printTerm,
  leafs,
  astVars,
) where

import Data.Dynamic (Typeable)
import Data.Either (rights)

import Test.IOTasks.Environment (Varname, Environment)

class Term (t :: * -> * -> *) where
  getCurrent' :: (Typeable a, UsageVar v) => Varname -> t v a
  getAll' :: (Typeable a, UsageVar v) => Varname -> t v [a]

class UsageVar v where
  addUsageInfo :: Varname -> Usage -> v

-- convenience reordering of quantification
getCurrent :: forall a t v. (Term t, Typeable a, UsageVar v) => Varname -> t v a
getCurrent = getCurrent'

getAll :: forall a t v. (Term t, Typeable a, UsageVar v) => Varname -> t v [a]
getAll = getAll'

-- alternative name for getCurrent
var :: forall a t v. (Term t, Typeable a, UsageVar v) => Varname -> t v a
var = getCurrent

class TermVars (t :: * -> * -> *) where
  termVars :: t v a -> [v]

-- syntactic terms
class SynTerm (t :: * -> * -> *) where
  viewTerm :: t v a -> AST v

type SpecVar a = (a,Usage)
data Usage = C | A  deriving (Show, Eq, Ord, Bounded)

-- semantic terms
class SemTerm (t :: * -> * -> *) where
  evalTerm :: t v a -> Environment -> a

-- Printable representation of terms
data AST v
  = Node String [AST v]
  | Infix (AST v) String (AST v)
  | Lam [Varname] (AST v)
  | Literal String
  | Var v
  deriving (Eq,Show)

leafs :: AST v -> [Either String v]
leafs (Node _ ts) = concatMap leafs ts
leafs (Infix l _ r) = leafs l ++ leafs r
leafs (Lam _ t) = leafs t
leafs (Literal x) = [Left x]
leafs (Var x) = [Right x]

astVars :: AST v -> [v]
astVars = rights . leafs

printAST :: Show v => AST v -> String
printAST = printAST' "" ""
  where
    printAST' o c (Node f xs) = o ++ f ++ " " ++ unwords (fmap (printAST' "(" ")") xs) ++ c
    printAST' o c (Infix x op y) = o ++ printAST x ++ " " ++ op ++ " " ++ printAST y ++ c
    printAST' o c (Lam vs b) = o ++ "\\" ++ unwords vs ++ " -> " ++ printAST b ++ c
    printAST' _ _ (Literal x) = x
    printAST' _ _ (Var x) = show x

printTerm :: (SynTerm t, Show v) => t v a -> String
printTerm = printAST . viewTerm
