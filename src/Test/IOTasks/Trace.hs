{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.Trace (
  -- * Trace class
  Trace(..),
  GeneralizedTrace(..),

  -- * LineTrace
  LineTrace,

  -- ** Patterns
  pattern ReadLine, pattern WriteLine, pattern LTerminate, pattern LOutOfInputs,
  isTerminatingLineTrace,

  -- ** Pretty printing
  printLineTrace, printLineTrace',

  -- * NormalizedTrace
  NormalizedTrace,
  NTracePlain,NTraceGeneralized,
  OptFlag(..),

  -- ** Patterns
  pattern NProgRead, pattern NProgWrite, pattern NTerminate, pattern NOutOfInputs,
  isTerminatingNTrace,
  -- ** Pretty printing
  printNTracePlain,
  printNTracePlain',
  printNTraceGeneralized, printNTraceGeneralizedSimple,
  printNTraceGeneralized', printNTraceGeneralizedSimple',

  -- * Trace matching
  covers,
  MatchResult,
  isSuccessfulMatch, isInputMismatch, isOutputMismatch, isAlignmentMismatch, isTerminationMismatch,
  printMatchResult, printMatchResultSimple,
) where

import Test.IOTasks.Trace.LineTrace as LT
import Test.IOTasks.Trace.NormalizedTrace as NT
import Test.IOTasks.Trace.Match
import Test.IOTasks.Trace.Class
