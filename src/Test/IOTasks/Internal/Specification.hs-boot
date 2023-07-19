{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Internal.Specification (
  Specification(..),
  readInput,writeOutput,writeOptionalOutput,optionalTextOutput,branch,tillExit,exit, nop,
  vars,
  pPrintSpecification,
  InputMode(..),
  ) where

import Test.IOTasks.ValueSet
import Test.IOTasks.Term
import Test.IOTasks.Terms (Var (..), SomeVar)
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern

import Data.Set (Set)
import Type.Reflection (Typeable)

import Text.PrettyPrint (Doc)

data Specification where
  ReadInput :: (Typeable a,Read a,Show a) => Var a -> ValueSet a -> InputMode -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (OutputPattern 'SpecificationP) -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  TillE :: Specification -> Specification -> Specification
  E :: Specification

data InputMode = AssumeValid | UntilValid | Abort

instance Semigroup Specification
instance Monoid Specification

readInput :: (Typeable a,Read a,Show a) => Var a -> ValueSet a -> InputMode -> Specification
writeOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOptionalOutput :: [OutputPattern 'SpecificationP] -> Specification
optionalTextOutput :: Specification
branch :: Term Bool -> Specification -> Specification -> Specification
nop :: Specification
tillExit :: Specification -> Specification
exit :: Specification

vars :: Specification -> [SomeVar]

pPrintSpecification :: Specification -> Doc
