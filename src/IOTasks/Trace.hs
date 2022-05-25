{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module IOTasks.Trace where

import IOTasks.OutputPattern

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Function (fix)

import Text.PrettyPrint hiding ((<>))

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
  | InputMismatch Trace Trace
  | OutputMismatch Trace Trace
  | AlignmentMismatch Trace (Maybe Trace) Trace
  | TerminationMismatch Trace (Maybe Trace) Trace
  deriving (Show, Eq)

instance Semigroup MatchResult where
  MatchSuccessfull <> _ = MatchSuccessfull
  _ <> MatchSuccessfull = MatchSuccessfull
  _ <> r = r

addExpect :: Trace -> MatchResult -> MatchResult
addExpect s' (AlignmentMismatch t _ s) = AlignmentMismatch t (Just s') s
addExpect s' (TerminationMismatch t _ s) = TerminationMismatch t (Just s') s
addExpect _ r = r

covers :: Trace -> Trace -> MatchResult
covers s@(ProgRead i t1) t@(ProgRead j t2)
  | i == j = t1 `covers` t2
  | otherwise = InputMismatch s t

covers s@(ProgWrite Mandatory is t1) t@(ProgWrite Mandatory js t2)
  | all (\j -> any (>: j) is) js = t1 `covers` t2
  | otherwise = OutputMismatch s t

covers s@(ProgWrite Optional is t1) t = ProgWrite Mandatory is t1 `covers` t <> addExpect s (t1 `covers` t)
covers s t@(ProgWrite Optional _ _) = OutputMismatch s t

covers Terminate Terminate = MatchSuccessfull
covers s@Terminate t = TerminationMismatch s Nothing t

covers OutOfInputs OutOfInputs = MatchSuccessfull

covers s t = AlignmentMismatch s Nothing t

pPrintMatchResult :: MatchResult -> Doc
pPrintMatchResult = pPrintMatchResult' False

pPrintMatchResultSimple :: MatchResult -> Doc
pPrintMatchResultSimple = pPrintMatchResult' True

pPrintMatchResult' :: Bool -> MatchResult -> Doc
pPrintMatchResult' _ MatchSuccessfull = text "MatchSuccessfull"
pPrintMatchResult' simple (InputMismatch s t) = text "InputMismatch:" $$ nest 2 (reportMismatch simple s Nothing t)
pPrintMatchResult' simple (OutputMismatch s t) = text "OutputMismatch:" $$ nest 2 (reportOutputMismatch simple s t)
pPrintMatchResult' simple (AlignmentMismatch s s' t) = text "AlignmentMismatch:" $$ nest 2 (reportMismatch simple s s' t)
pPrintMatchResult' simple (TerminationMismatch s s' t) = text "TerminationMismatch:" $$ nest 2 (reportMismatch simple s s' t)


reportMismatch :: Bool -> Trace -> Maybe Trace -> Trace -> Doc
reportMismatch simple s s' t = vcat
  [ text "Expected:"
  , nest 2 (maybe mempty ((<+> text "or") . showTraceHead simple) s' <+> showTraceHead simple s)
  , text "Got:"
  , nest 2 (showTraceHead simple t)
  ]

reportOutputMismatch :: Bool -> Trace -> Trace -> Doc
reportOutputMismatch simple s t = showTraceHead simple t <+> text "is not covered by" <+> showTraceHead simple s

showTraceHead :: Bool -> Trace -> Doc
showTraceHead simple = text . showTraceHead' simple (const "")

pPrintTrace :: Trace -> String
pPrintTrace = fix (showTraceHead' False)

pPrintTraceSimple :: Trace -> String
pPrintTraceSimple = fix (showTraceHead' True)

showTraceHead' :: Bool -> (Trace -> String) -> Trace -> String
showTraceHead' simple f (ProgRead x (ProgRead '\n' t)) = "?"++ [x] ++ (if simple then "" else "\\n") ++ addSpace (f t)
showTraceHead' simple f (ProgRead x (ProgRead c t)) = "?"++ x : tail (showTraceHead' simple f (ProgRead c t))
showTraceHead' _ f (ProgRead x t') = "?"++[x] ++ addSpace (f t')
showTraceHead' simple f (ProgWrite Optional ts t')
  | simple = "(!"++ (head $ printPatternSimple <$> Set.toList ts) ++ ")" ++ addSpace (f t') -- omit optional outputs in simplified version
  | otherwise  = "(!{"++ intercalate "," (printPattern <$> Set.toList ts) ++ "})" ++ addSpace (f t')
showTraceHead' simple f (ProgWrite Mandatory ts t')
  | simple = "!"++ (head $ printPatternSimple <$> Set.toList ts) ++ addSpace (f t')
  | otherwise = "!{"++ intercalate "," (printPattern <$> Set.toList ts) ++ "}" ++ addSpace (f t')
showTraceHead' _ _ Terminate = "stop"
showTraceHead' _ _ OutOfInputs = "?<unknown input>"

addSpace :: String -> String
addSpace "" = ""
addSpace s = ' ':s

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
