{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.Trace (
  -- * Trace class
  Trace(..),
  ConcreteTrace(..),
  GeneralizedTrace(..),

  -- * DirectTrace
  DirectTrace(..),
  isTerminatingDirectTrace,

  -- * LineTrace
  LineTrace,

  -- ** Patterns
  pattern ReadLine, pattern WriteLine, pattern LTerminate, pattern LOutOfInputs,
  isTerminatingLineTrace,

  -- * NormalizedTrace
  NormalizedTrace,
  NTracePlain,NTraceGeneralized,
  OptFlag(..),

  -- ** Patterns
  pattern NProgRead, pattern NProgWrite, pattern NTerminate, pattern NOutOfInputs,
  isTerminatingNTrace,

  -- * Trace matching
  covers,
  MatchResult,
  isSuccessfulMatch, isInputMismatch, isOutputMismatch, isAlignmentMismatch, isTerminationMismatch,
  printMatchResult, printMatchResultSimple,

  -- * Pretty Printing
  PrettyPrintTrace(..),
  PrettyPrintWrite(..),
  printTrace, printTrace',
  printTraceSimple, printTraceSimple',
  printTraceHead, printTraceHeadSimple,
) where

import Test.IOTasks.Trace.DirectTrace
import Test.IOTasks.Trace.LineTrace
import Test.IOTasks.Trace.NormalizedTrace
import Test.IOTasks.Trace.Match
import Test.IOTasks.Trace.Class
import Test.IOTasks.Trace.PrettyPrint
