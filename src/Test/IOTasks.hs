module Test.IOTasks (
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
  OutputPattern,
  wildcard, text, value,
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
  generateStaticTestSuite, taskCheckOn,
  -- * Interpreter
  interpret,
  ) where

import Prelude hiding (until)

import Test.IOTasks.Specification
import Test.IOTasks.MonadTeletype
import Test.IOTasks.IOrep
import Test.IOTasks.ConditionTerm
import Test.IOTasks.Terms
import Test.IOTasks.ValueSet
import Test.IOTasks.OutputPattern
import Test.IOTasks.OutputTerm
import Test.IOTasks.Testing
import Test.IOTasks.Interpreter
import Test.IOTasks.Trace
