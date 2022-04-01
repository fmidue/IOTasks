module Trace where

data Trace
  = ProgRead Integer Trace
  | ProgWrite Integer Trace
  | Terminate
  deriving (Eq,Show)
