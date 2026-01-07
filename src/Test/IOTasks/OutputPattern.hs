module Test.IOTasks.OutputPattern (
  PatternKind (..),
  OutputPattern,
  wildcard, nonEmptyWildcard, text, resultOf, decoratedResultOf, emptyPattern,
  valueTerms,
  showPattern, showPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
