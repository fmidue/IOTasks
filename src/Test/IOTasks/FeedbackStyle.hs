module Test.IOTasks.FeedbackStyle (
  FeedbackStyle(..),
  TraceStyle(..),
  defaultFeedback,
  ) where

data FeedbackStyle = FeedbackStyle
  -- | cleanup feedback for educational use
  { simplifyFeedback :: Bool
  -- | trace printing style
  , traceStyle :: TraceStyle
  }

data TraceStyle = HorizontalTrace | VerticalTrace

defaultFeedback :: FeedbackStyle
defaultFeedback = FeedbackStyle { simplifyFeedback = False, traceStyle = HorizontalTrace }
