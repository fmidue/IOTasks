{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Internal.Specification where

import Test.IOTasks.ValueSet
import Test.IOTasks.Term
import Test.IOTasks.Terms (Var (..), varname)
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern

import Data.Set (Set)
import Data.Typeable (Typeable)

import Text.PrettyPrint (Doc)


data Specification where
  ReadInput :: (Typeable a,Read a,Show a) => Var -> ValueSet a -> InputMode -> Specification -> Specification
  WriteOutput :: OptFlag -> Set (OutputPattern 'SpecificationP) -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification
  TillE :: Specification -> Specification -> Specification
  E :: Specification

instance Semigroup Specification
instance Monoid Specification

data InputMode = AssumeValid | UntilValid | Abort

readInput :: (Typeable a,Read a,Show a) => Var -> ValueSet a -> InputMode -> Specification
writeOutput :: [OutputPattern 'SpecificationP] -> Specification
writeOptionalOutput :: [OutputPattern 'SpecificationP] -> Specification
optionalTextOutput :: Specification
branch :: Term Bool -> Specification -> Specification -> Specification
nop :: Specification
tillExit :: Specification -> Specification
exit :: Specification

vars :: Specification -> [Var]

pPrintSpecification :: Specification -> Doc
