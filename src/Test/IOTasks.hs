module Test.IOTasks
  ( Specification, runSpecification, runSpecification', accept
  , readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, until, while
  , pPrintSpecification
  , InputMode(..)
  , MonadTeletype(..)
  , IOrep, runProgram, Line
  , Trace, (>:)
  , Term
  , ValueSet(..)
  , ints, nats, str
  , OutputPattern(..), PatternType(..)
  , OutputTerm, filter'
  , Var, Varname, var, intVar, stringVar
  , (.+.), (.-.), (.*.), intLit
  , (.==.), (.>.), (.>=.), (.<.), (.<=.)
  , (.&&.), (.||.), not', true, false
  , sum', length', reverse', product', listLit
  , isIn, isNotIn
  , currentValue, allValues, currentValue', allValues', as
  , OverflowType
  , taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs
  , Outcome(..), CoreOutcome(..), OutcomeHints(..), isSuccess, isFailure, overflowWarnings
  , pPrintOutcome, pPrintOutcomeSimple
  , generateStaticTestSuite, taskCheckOn
  , interpret
  ) where

import Prelude hiding (until)

import Test.IOTasks.Specification
import Test.IOTasks.MonadTeletype
import Test.IOTasks.IOrep
import Test.IOTasks.Term
import Test.IOTasks.Terms
import Test.IOTasks.ValueSet
import Test.IOTasks.OutputPattern
import Test.IOTasks.OutputTerm
import Test.IOTasks.Testing
import Test.IOTasks.Interpreter
import Test.IOTasks.Trace
import Test.IOTasks.Overflow (OverflowType)
