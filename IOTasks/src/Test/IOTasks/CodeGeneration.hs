module Test.IOTasks.CodeGeneration (
  IRProgram,
  programIR,
  programIR',
  haskellCode,
  pseudoCode,
  programVariants,
  FreshVarM,
  runFreshVarM,
  emptyVarInfo,
  ) where

import Test.IOTasks.CodeGeneration.Translation
import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.Optimization
import Test.IOTasks.CodeGeneration.Render
import Test.IOTasks.CodeGeneration.FreshVar
