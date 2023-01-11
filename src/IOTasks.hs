module IOTasks
  ( Specification, runSpecification, runSpecification', accept
  , readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, nop, until, while
  , pPrintSpecification
  , InputMode(..)
  , MonadTeletype(..)
  , IOrep, runProgram, Line
  , Trace, (>:)
  , Term
  , ValueSet(..)
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
  , Outcome(..), CoreOutcome(..), OutcomeHints(..), isSuccess, overflowWarnings
  , pPrintOutcome, pPrintOutcomeSimple
  , generateStaticTestSuite, taskCheckOn
  , interpret
  ) where

import Prelude hiding (until)

import IOTasks.Specification
import IOTasks.MonadTeletype
import IOTasks.IOrep
import IOTasks.Term
import IOTasks.Terms
import IOTasks.ValueSet
import IOTasks.OutputPattern
import IOTasks.OutputTerm
import IOTasks.Testing
import IOTasks.Interpreter
import IOTasks.Trace
import IOTasks.Overflow (OverflowType)
