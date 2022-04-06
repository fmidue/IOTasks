module Trace where

import Data.Set

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

data Trace
  = ProgRead Integer Trace
  | ProgWrite OptFlag (Set Integer) Trace
  | Terminate
  | OutOfInputs
  deriving (Eq, Show)

data MatchResult
  = MatchSuccessfull
  | InputMismatch String
  | OutputMismatch String
  | AlignmentMismatch String
  | TerminationMismatch String
  deriving (Show, Eq)

instance Semigroup MatchResult where
  MatchSuccessfull <> _ = MatchSuccessfull
  _ <> MatchSuccessfull = MatchSuccessfull
  _ <> r = r

covers :: Trace -> Trace -> MatchResult
covers s@(ProgRead i t1) t@(ProgRead j t2)
  | i == j = t1 `covers` t2
  | otherwise = InputMismatch $ reportMismatch s t

covers s@(ProgWrite Mandatory is t1) t@(ProgWrite Mandatory js t2)
  | js `isSubsetOf` is = t1 `covers` t2
  | otherwise = OutputMismatch $ reportMismatch s t

covers (ProgWrite Optional is t1) t = ProgWrite Mandatory is t1 `covers` t <> t1 `covers` t
covers s (ProgWrite Optional is t2) = s `covers` ProgWrite Mandatory is t2 <> s `covers` t2

covers Terminate Terminate = MatchSuccessfull
covers s@Terminate t = TerminationMismatch $ reportMismatch s t

covers OutOfInputs OutOfInputs = MatchSuccessfull

covers s t = AlignmentMismatch $ reportMismatch s t

reportMismatch :: Trace -> Trace -> String
reportMismatch s t = unwords ["Expected:",showTraceHead s,"Got:",showTraceHead t]

showTraceHead :: Trace -> String
showTraceHead (ProgRead x _) = "?"++show x
showTraceHead (ProgWrite Optional ts _) = "(!"++show (toList ts)++")"
showTraceHead (ProgWrite Mandatory ts _) = "!"++show (toList ts)
showTraceHead Terminate = "stop"
showTraceHead OutOfInputs = "?<unknown input>"
