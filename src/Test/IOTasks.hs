module Test.IOTasks (
  -- * Specifications
  Specification, runSpecification, runSpecification', accept,
  readInput,
  writeOutput, writeOptionalOutput, anyOptionalOutput, branch, tillExit, exit, nop, while, whileNot, repeatUntil, doWhile,
  InputMode(..),
  ValueSet,
  empty, complete, singleton, fromList,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  complement,
  unique, notInVar,
  isEmpty,
  ints, nats, str,
  OutputPattern,
  wildcard, text, resultOf,
  Varname, Var, var, intVar, stringVar,
  pPrintSpecification,
  -- * Terms
  Term,
  TermKind(..),
  as,
  -- ** Accessors
  currentValue, allValues,
  valueBefore, valuesBefore,
  -- ** Arithmetic functions
  (.+.), (.-.), (.*.),
  intLit,
  -- ** Comparison functions
  (.==.), (./=.), (.>.), (.>=.), (.<.), (.<=.),
  -- ** Boolean functions
  not',
  (.&&.), (.||.),
  true, false,
  -- ** Simple list functions
  sum', product', length', reverse',
  isIn, isNotIn,
  listLit,
  -- ** Complexer list functions
  filter',
  -- ** Lifting of opaque functions
  liftOpaqueValue, liftOpaque, liftOpaque2,
  -- * Programs
  MonadTeletype(..),
  IOrep, runProgram, Line,
  Trace, covers,
  -- * Testing
  taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs,
  Outcome(..), CoreOutcome(..), OutcomeHints(..), isSuccess, isFailure, overflowWarnings,
  FeedbackStyle(..), TraceStyle(..), defaultFeedback,
  printOutcomeWith,
  -- ** Pre-computed test suites
  generateStaticTestSuite, taskCheckOn,
  -- * Interpreter
  interpret,
  ) where

import Test.IOTasks.Interpreter
import Test.IOTasks.IOrep
import Test.IOTasks.OutputPattern
import Test.IOTasks.Specification
import Test.IOTasks.Term
import Test.IOTasks.Term.Prelude
import Test.IOTasks.Testing
import Test.IOTasks.Trace
import Test.IOTasks.ValueSet
import Test.IOTasks.Var
