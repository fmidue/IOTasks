module Trace where

data Trace
  = ProgRead Integer Trace
  | ProgWrite Integer Trace
  | Terminate
  | OutOfInputs
  deriving (Eq,Show)
