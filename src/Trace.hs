{-# LANGUAGE DataKinds #-}
module Trace where

import OutputPattern

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Function (fix)

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

data Trace
  = ProgRead Char Trace
  | ProgWrite OptFlag (Set (OutputPattern 'TraceP)) Trace
  | Terminate
  | OutOfInputs
  deriving (Eq, Show)

progRead :: Char -> Trace
progRead c = ProgRead c Terminate

progWrite :: OptFlag -> Set (OutputPattern 'TraceP) -> Trace
progWrite o ts = ProgWrite o ts Terminate

instance Semigroup Trace where
  ProgRead c t <> t' = ProgRead c $ t <> t'
  ProgWrite o1 ts1 t <> ProgWrite o2 ts2 t'
    = ProgWrite
      (max o1 o2)
      (Set.unions $
        [ ts1 | o2 == Optional ] ++
        [ ts2 | o1 == Optional ] ++
        [Set.map (uncurry (<>)) $ Set.cartesianProduct ts1 ts2]) t
      <> t'
  ProgWrite o ts t <> t' = ProgWrite o ts $ t <> t'
  Terminate <> t' = t'
  OutOfInputs <> _ = OutOfInputs

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
  | all (\j -> any (>: j) is) js = t1 `covers` t2
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
showTraceHead = showTraceHead' (const "")

pPrintTrace :: Trace -> String
pPrintTrace = fix showTraceHead'

showTraceHead' :: (Trace -> String) -> Trace -> String
showTraceHead' f (ProgRead x (ProgRead '\n' t)) = "?"++ [x] ++ "\\n" ++ addSpace (showTraceHead' f t)
showTraceHead' f (ProgRead x (ProgRead c t)) = "?"++ x : tail (showTraceHead' f (ProgRead c t))
showTraceHead' f (ProgRead x t') = "?"++[x] ++ addSpace (f t')
showTraceHead' f (ProgWrite Optional ts t') = "(!["++ intercalate "," (printPattern <$> Set.toList ts) ++ "])" ++ addSpace (f t')
showTraceHead' f (ProgWrite Mandatory ts t') = "!["++ intercalate "," (printPattern <$> Set.toList ts) ++ "]" ++ addSpace (f t')
showTraceHead' _ Terminate = "stop"
showTraceHead' _ OutOfInputs = "?<unknown input>"

addSpace :: String -> String
addSpace "" = ""
addSpace s = ' ':s

pPrintMatchResult :: MatchResult -> String
pPrintMatchResult MatchSuccessfull = "MatchSuccessfull"
pPrintMatchResult (InputMismatch s) = unlines ["InputMismatch:",s]
pPrintMatchResult (OutputMismatch s) = unlines ["OutputMismatch:",s]
pPrintMatchResult (AlignmentMismatch s) = unlines ["AlignmentMismatch:",s]
pPrintMatchResult (TerminationMismatch s) = unlines ["TerminationMismatch:",s]

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
