module Test.IOTasks.Random (
  -- * Specifications
  Specification, runSpecification, runSpecification', accept,
  readInput,
  writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, until, while,
  InputMode(..), ValueSet(..),
  ints, nats, str,
  OutputPattern(..),
  SomeVar, Varname, var, intVar, stringVar,
  pPrintSpecification,
  -- * Terms
  ConditionTerm,
  OutputTerm,
  Arithmetic(..),
  Compare(..),
  Logic(..),
  BasicLists(..), ComplexLists(..),
  Sets(..),
  Accessor(..),
  as,
  OverflowType,
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
