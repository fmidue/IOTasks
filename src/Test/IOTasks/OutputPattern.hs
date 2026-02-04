module Test.IOTasks.OutputPattern (
  PatternKind (..),
  OutputPattern,
  wildcard, nonEmptyWildcard, text, resultOf, decoratedResultOf, emptyPattern,
  valueTerms,
  canBeEmpty,
  showPattern, showPatternSimple,
  evalPattern,
  (>:),
  ) where

import Test.IOTasks.Internal.OutputPattern
