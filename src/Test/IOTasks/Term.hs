module Test.IOTasks.Term (
  Term,
  TermKind(..),
  oEval,
  termVarExps,
  transparentSubterms,
  showTerm, showIndexedTerm,
  SomeTerm(..), withSomeTerm,
  SomeTermK(..), withSomeTermK,
  castTerm,
  ) where

import Test.IOTasks.Internal.Term
