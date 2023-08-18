module Test.IOTasks.Random (
  -- * Specifications
  Specification, runSpecification, runSpecification', accept,
  readInput,
  writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, while, whileNot, repeatUntil, doWhile,
  InputMode(..),
  ValueSet,
  empty, complete, singleton, fromList,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  complement,
  isEmpty,
  ints, nats, str,
  OutputPattern,
  wildcard, text, value,
  Varname, Var, var, intVar, stringVar,
  pPrintSpecification,
  -- * Terms
  ConditionTerm,
  OutputTerm,
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
  pPrintOutcome, pPrintOutcomeSimple,
  -- ** Pre-computed test suites
  genInput, taskCheckOn,
  -- * Interpreter
  interpret,
  ) where

import Test.IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import Test.IOTasks.Random.Testing
