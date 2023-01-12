module IOTasks.Random
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
  , (.&&.), (.||.), not'
  , sum', length', product'
  , currentValue, allValues
  , taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs
  , Outcome(..), isSuccess, overflowWarnings
  , pPrintOutcome, pPrintOutcomeSimple
  , generateStaticTestSuite, taskCheckOn
  , genInput
  , interpret
  ) where

import Prelude hiding (until)

import IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import IOTasks.Random.Testing
