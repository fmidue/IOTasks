module Trace where

data Trace
  = ProgRead Int Trace
  | ProgWrite Int Trace
  | Terminate
  deriving Show
