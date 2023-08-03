module Test.IOTasks.Random (
  -- * Specifications
  Specification, runSpecification, runSpecification', accept,
  readInput,
  writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, while, whileNot,
  InputMode(..),
  ValueSet,
  empty, every, singleton,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  complement,
  ints, nats, str,
  OutputPattern(..),
  Varname, var, intVar, stringVar,
  pPrintSpecification,
  -- * Terms
  ConditionTerm,
  OutputTerm,
  Arithmetic(..),
  Compare(..),
  Logic(..),
  BasicLists(..), ComplexLists(..),
  Membership(..),
  Accessor(..),
  as,
  -- * Programs
  MonadTeletype(..),
  IOrep, runProgram, Line,
  Trace, covers,
  -- * Testing
  taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs,
  Outcome(..), CoreOutcome(..), OutcomeHints(..), isSuccess, isFailure, overflowWarnings,
  pPrintOutcome, pPrintOutcomeSimple,
  -- ** pre-computed test suites
  genInput, taskCheckOn,
  -- * Interpreter
  interpret,
  ) where

import Prelude hiding (until)

import Test.IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import Test.IOTasks.Random.Testing
