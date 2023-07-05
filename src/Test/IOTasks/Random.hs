module Test.IOTasks.Random (
  Specification, runSpecification, runSpecification', accept,
  readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, until, while,
  pPrintSpecification,
  InputMode(..),
  MonadTeletype(..),
  IOrep, runProgram, Line,
  Trace, (>:),
  Term,
  ValueSet(..),
  ints, nats, str,
  OutputPattern(..), PatternType(..),
  OutputTerm,
  Var, Varname, var, intVar, stringVar,
  Arithmetic(..),
  Compare(..),
  Logic(..),
  BasicLists(..), ComplexLists(..),
  Sets(..),
  Accessor(..),
  as,
  OverflowType,
  taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs,
  Outcome(..), CoreOutcome(..), OutcomeHints(..), isSuccess, isFailure, overflowWarnings,
  pPrintOutcome, pPrintOutcomeSimple,
  generateStaticTestSuite, taskCheckOn,
  interpret,
  ) where

import Prelude hiding (until)

import Test.IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import Test.IOTasks.Random.Testing
