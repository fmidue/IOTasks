module Test.IOTasks.OutputPattern (
  PatternKind (..),
  OutputPattern,
  wildcard, text, resultOf,
  valueTerms,
  showPattern, showPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
