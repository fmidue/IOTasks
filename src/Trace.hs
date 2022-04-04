module Trace where

import Data.Set

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

data Trace
  = ProgRead Integer Trace
  | ProgWrite OptFlag (Set Integer) Trace
  | Terminate
  | OutOfInputs
  deriving (Eq, Show)

covers :: Trace -> Trace -> Bool
covers (ProgRead i t1) (ProgRead j t2) = i == j && t1 `covers` t2
covers (ProgWrite Mandatory is t1) (ProgWrite Mandatory js t2) =
  js `isSubsetOf` is && t1 `covers` t2
covers (ProgWrite Optional is t1) t = ProgWrite Mandatory is t1 `covers` t || t1 `covers` t
covers s (ProgWrite Optional is t2) = s `covers` ProgWrite Mandatory is t2 || s `covers` t2 
covers Terminate Terminate = True
covers OutOfInputs OutOfInputs = True
covers _ _ = False
