module Test.IOTasks.Term (
  Term,
  eval,
  termVarExps, subTerms,
  printTerm, printIndexedTerm,
  SomeTerm(..),
  castTerm,
  ) where

import Test.IOTasks.Internal.Term
