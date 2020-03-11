{-# LANGUAGE NoImplicitPrelude #-}
module Test.IOTasks (
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
  FixedPattern, buildPattern,
  TermPattern, buildTermPattern,
  Pattern(..), var,
  ValueSet,
  intValues,
  values,
  stringValues,
  mkValueSet,
  ints,
  nats,
  StringEmbedding,
  -- * Combinators
  readTillFixedLength,
  readUntil,
  repeatSpec,
  when,
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
  IOTasksable(..),
  accept,
  matchesTrace,
  MatchResult(..),
  -- * Artifacts
  buildComputation,
  pythonCode,
) where

import Test.IOTasks.Language
import Test.IOTasks.Combinators
import Test.IOTasks.Term
import Test.IOTasks.IOrep
import Test.IOTasks.IOProperty
import Test.IOTasks.Artifacts
