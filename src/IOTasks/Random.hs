module IOTasks.Random
  ( Specification, runSpecification
  , readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, until
  , InputMode(..)
  , MonadTeletype(..)
  , IOrep, runProgram, Line
  , Trace, (>:)
  , Term(..)
  , ValueSet(..)
  , OutputPattern(..), PatternType(..)
  , OutputTerm, current, all, (+#), (-#), (*#), length', sum'
  , taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs
  , generateStaticTestSuite, taskCheckOn
  , interpret
  ) where

import Prelude hiding (until,all)

import IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import IOTasks.Random.Testing
