module IOTasks.Random
  ( Specification, runSpecification
  , readInput, writeOutput, writeOptionalOutput, optionalTextOutput, branch, tillExit, exit, until, while
  , InputMode(..)
  , MonadTeletype(..)
  , IOrep, runProgram, Line
  , Trace, (>:)
  , Term(..)
  , ValueSet(..)
  , OutputPattern(..), PatternType(..)
  , OutputTerm, current, all, (+#), (-#), (*#), length', sum'
  , taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs,  Outcome(..)
  , generateStaticTestSuite, taskCheckOn
  , interpret
  ) where

import Prelude hiding (until,all)

import IOTasks hiding (taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome, Args(..), stdArgs)
import IOTasks.Random.Testing
