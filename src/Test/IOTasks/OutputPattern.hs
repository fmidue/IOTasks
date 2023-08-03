module Test.IOTasks.OutputPattern (
  PatternType (..),
  OutputPattern,
  wildcard, text, value,
  valueTerms,
  printPattern, printPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
