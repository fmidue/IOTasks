module Test.IOTasks.Specification (
  Specification,
  readInput, writeOutput, writeOptionalOutput, optionalTextOutput,
  branch, tillExit, exit, while, until, nop,
  runSpecification,
  runSpecification', AddLinebreaks,
  vars, hasIteration,
  pPrintSpecification,
  InputMode(..),
  accept,
  LoopBody(..),
  ) where

import Prelude hiding (until)
import Test.IOTasks.Internal.Specification
