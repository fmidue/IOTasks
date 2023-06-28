module IOTasks.Random
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
  , genInput
  , interpret
  ) where

import Prelude hiding (until)

import IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import IOTasks.Random.Testing
