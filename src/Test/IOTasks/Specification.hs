module Test.IOTasks.Specification (
  Specification,
  readInput, writeOutput, writeOptionalOutput, anyOptionalOutput,
  branch, tillExit, exit, while, whileNot, repeatUntil, doWhile, nop,
  runSpecification,
  runSpecification', AddLinebreaks,
  readVars, hasIteration,
  pPrintSpecification,
  InputMode(..),
  accept,
  LoopBody(..),
  ) where

import Test.IOTasks.Internal.Specification
