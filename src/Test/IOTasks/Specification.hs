module Test.IOTasks.Specification (
  Specification,
  readInput, writeOutput, writeOptionalOutput, optionalTextOutput,
  branch, tillExit, exit, while, whileNot, nop,
  runSpecification,
  runSpecification', AddLinebreaks,
  vars, hasIteration,
  pPrintSpecification,
  InputMode(..),
  accept,
  LoopBody(..),
  ) where

import Test.IOTasks.Internal.Specification
