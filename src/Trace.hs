module Trace where

import Data.Set (Set, toList, isSubsetOf)
import Data.List (intersperse, intercalate)

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

data Trace
  = ProgRead Char Trace
  | ProgWrite OptFlag (Set String) Trace
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
showTraceHead (ProgRead x (ProgRead c t)) | c /= '\n' = "?"++ x : tail (showTraceHead (ProgRead c t))
showTraceHead (ProgRead x _) = "?"++[x]
showTraceHead (ProgWrite Optional ts _) = "(!["++ intercalate "," (toList ts) ++ "])"
showTraceHead (ProgWrite Mandatory ts _) = "!["++ intercalate "," (toList ts) ++ "]"
showTraceHead Terminate = "stop"
showTraceHead OutOfInputs = "?<unknown input>"

isTerminating :: Trace -> Bool
isTerminating (ProgRead _ t) = isTerminating t
isTerminating (ProgWrite _ _ t) = isTerminating t
isTerminating Terminate = True
isTerminating OutOfInputs = False

inputSequence :: Trace -> [String]
inputSequence = go "" where
  go cs (ProgRead '\n' t) = reverse cs : go "" t
  go cs (ProgRead c t) = go (c:cs) t
  go "" (ProgWrite _ _ t) = go "" t
  go cs (ProgWrite _ _ t) = reverse cs : go "" t
  -- technically this might add an additional linebreak on the last line that might not be there in the Trace
  go "" Terminate = []
  go cs Terminate = [reverse cs]
  go "" OutOfInputs = []
  go cs OutOfInputs = [reverse cs]
