module Test.IOTasks.OutputPattern (
  PatternKind (..),
  OutputPattern,
  wildcard, text, value,
  valueTerms,
  showPattern, showPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
