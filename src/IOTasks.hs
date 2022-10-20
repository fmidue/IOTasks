module IOTasks
  ( Specification, runSpecification
  , readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, until, while
  , InputMode(..)
  , MonadTeletype(..)
  , IOrep, runProgram, Line
  , Trace, (>:)
  , Term
  , ValueSet(..)
  , OutputPattern(..), PatternType(..)
  , OutputTerm, filter'
  , (.+.), (.-.), (.*.), intLit
  , (.==.), (.>.), (.>=.), (.<.), (.<=.)
  , (.&&.), (.||.), not', true, false
  , sum', length', product', listLit
  , isIn, isNotIn
  , currentValue, allValues, currentValue', allValues'
  , taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs
  , Outcome(..), isSuccess, overflowWarnings 
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
