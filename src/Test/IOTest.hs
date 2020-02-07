{-# LANGUAGE NoImplicitPrelude #-}
module Test.IOTest (
  -- * Language
  Specification,
  readInput,
  writeOutput,
  branch,
  tillExit,
  nop,
  exit,
  writeFixedOutput,
  Varname, optional,
  Pattern, buildPattern,
  TermPattern, buildTermPattern,
  ValueSet,
  intValues,
  values,
  mkValueSet,
  ints,
  nats,
  StringEmbedding,
  -- * Terms
  Term(..),
  getCurrent,
  getAll,
  TermVars(..),
  SynTerm(..),
  SemTerm(..),
  AST(..),
  printAST,
  printTerm,
  -- * IOrep
  IOrep,
  IOrep'(..),
  runProgram,
  MonadTeletype(..), print, readLn,
  -- * IOProperty
  IOTestable(..),
  accept,
  matchesTrace,
  MatchResult(..),
  -- * Artifacts
  buildComputation,
  pythonCode,
) where

import Test.IOTest.Language
import Test.IOTest.Term
import Test.IOTest.IOrep
import Test.IOTest.IOProperty
import Test.IOTest.Artifacts
