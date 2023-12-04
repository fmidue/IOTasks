{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Internal.Specification (
  Specification(..),
  readInput,writeOutput,writeOptionalOutput,anyOptionalOutput,branch,tillExit,exit, nop,
  readVars,
  pPrintSpecification,
  InputMode(..),
  ) where

import Test.IOTasks.ValueSet
import Test.IOTasks.Term
import Test.IOTasks.Var (Var (..), SomeVar)
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern

import Data.Set (Set)
import Type.Reflection (Typeable)

import Text.PrettyPrint (Doc)

data Specification where
  ReadInput :: (Typeable a,Read a,Show a) => Var a -> ValueSet a -> InputMode -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (OutputPattern k) -> Specification -> Specification
  Branch :: Term 'Transparent Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  TillE :: Specification -> Specification -> Specification
  E :: Specification

data InputMode = AssumeValid | UntilValid | ElseAbort

instance Semigroup Specification
instance Monoid Specification

readInput :: (Typeable a,Read a,Show a) => Var a -> ValueSet a -> InputMode -> Specification
writeOutput :: [OutputPattern k] -> Specification
writeOptionalOutput :: [OutputPattern k] -> Specification
anyOptionalOutput :: Specification
branch :: Term 'Transparent Bool -> Specification -> Specification -> Specification
nop :: Specification
tillExit :: Specification -> Specification
exit :: Specification

readVars :: Specification -> [SomeVar]

pPrintSpecification :: Specification -> Doc
