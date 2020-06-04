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
  getCurrent, getAll,
  writeFixedOutput,
  Varname, optional,
  FixedPattern, buildPattern,
  TermPattern, buildTermPattern,
  Pattern(..), var, whitespace,
  ValueSet,
  intValues,
  values,
  stringValues,
  mkValueSet,
  ints,
  nats,
  StringEmbedding,
  SpecTerm,
  -- * Combinators
  readTillFixedLength,
  readUntil,
  repeatSpec,
  when,
  -- * IOrep
  IOrep(..),
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

import Test.IOTasks.Language
import Test.IOTasks.Combinators
import Test.IOTasks.IOrep
import Test.IOTasks.IOProperty
import Test.IOTasks.Artifacts
