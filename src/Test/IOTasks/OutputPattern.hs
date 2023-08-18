module Test.IOTasks.OutputPattern (
  PatternType (..),
  OutputPattern,
  wildcard, text, value,
  valueTerms,
  showPattern, showPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
