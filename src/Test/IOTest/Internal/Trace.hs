{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.Internal.Trace (
  NTrace,
  Trace,
  Trace'(..),
  normalize,
  isCoveredBy,
  ppNTrace,
  ppNTraceInfo,
  inputs,
  MatchResult(..),
  ppResult,
) where

import Test.IOTest.Internal.Pattern

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List
import           Data.Function ( fix )

import Text.PrettyPrint hiding ((<>))

data Trace' o
  = ProgRead String (Trace' o)
  | ProgWrite o (Trace' o)
  | Stop
  | OutOfInputs
  deriving Functor

type Trace = Trace' String
type NTrace = Trace' (Set LinearPattern)

normalize :: Trace -> NTrace
normalize = go emptyPattern where
  go p (ProgWrite v t') = go (p <> buildPattern v) t'
  go p (ProgRead v t') = ProgWrite (S.singleton p) $ ProgRead v $ go emptyPattern t'
  go p Stop = ProgWrite (S.singleton p) Stop
  go p OutOfInputs = ProgWrite (S.singleton p) OutOfInputs

data MatchResult
  = MatchSuccessfull
  | InputMismatch Doc
  | OutputMismatch Doc
  | AlignmentMismatch Doc
  deriving Show

ppResult :: MatchResult -> Doc
ppResult MatchSuccessfull = text $ show MatchSuccessfull
ppResult (InputMismatch msg) = hang (text "InputMismatch:") 4 msg
ppResult (OutputMismatch msg) = hang (text "OutputMismatch:") 4 msg
ppResult (AlignmentMismatch msg) = hang (text "AlignmentMismatch:") 4 msg

isCoveredBy :: NTrace -> NTrace -> MatchResult
t1@(ProgRead x t1') `isCoveredBy` t2@(ProgRead y t2') =
  if x == y
    then t1' `isCoveredBy` t2'
    else InputMismatch (reportExpectationMismatch t2 t1)
t1@(ProgWrite v1 t1') `isCoveredBy` (ProgWrite v2 t2')
  | v1 `isSubsetOf` v2 = t1' `isCoveredBy` t2'
  | S.size v2 == 1 && S.member emptyPattern v2 = InputMismatch $ reportExpectationMismatch t2' t1
  | otherwise = OutputMismatch $ reportCoverageFailure v1 v2
Stop `isCoveredBy` Stop = MatchSuccessfull
t1 `isCoveredBy` t2 = AlignmentMismatch (reportExpectationMismatch t1 t2)

isSubsetOf :: Set LinearPattern -> Set LinearPattern -> Bool
x `isSubsetOf` y = x == y || all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) (S.toList x)
                          -- '           '        '- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

reportCoverageFailure :: Set LinearPattern -> Set LinearPattern -> Doc
reportCoverageFailure xs ys = text "the set" <+> printSet xs <+> text "is not a subset of" <+> printSet ys
  where removeBrackets s = s >>= \c -> [c | c `notElem` "[]"]
        printSet set = braces . text $ removeBrackets $ show (S.toList set)

reportExpectationMismatch :: NTrace -> NTrace -> Doc
reportExpectationMismatch t1 t2 = text (showNTraceHead Expected t1) $$ text (showNTraceHead Got t2)

instance Show Trace where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

instance Show NTrace where
  show = fix showNTraceFF

showNTraceFF :: (NTrace -> String) -> NTrace -> String
showNTraceFF ff (ProgRead v t) = "?"++ v++" "++ ff t
showNTraceFF ff (ProgWrite v t) =
  let vs = (\x -> if x == "[]" then "e" else x) . show <$> S.toList v
  in "!{"++ intercalate "," vs ++"} "++ ff t
showNTraceFF _ Stop = "stop"
showNTraceFF _ OutOfInputs = "<out of inputs>"

data ShowHeadType = Expected | Got | Plain

showNTraceHead :: ShowHeadType -> NTrace -> String
showNTraceHead Expected t = "Expected: " <> showNTraceHead Plain t
showNTraceHead Got t = "Got: " <> showNTraceHead Plain t
showNTraceHead Plain t = showNTraceFF (const "") t

ppNTraceInfo :: NTrace -> Doc
ppNTraceInfo t = inputSequence $+$ generalizedRun
  where inputSequence = hang (text "Input sequence:") 4 $ hsep $ (text . ('?' :)) <$> inputs t
        generalizedRun = hang (text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: NTrace -> Doc
ppNTrace = hcat . fmap text . filter (/= "!{} ") . traceToStringSequence

traceToStringSequence :: NTrace -> [String]
traceToStringSequence t@(ProgRead _ t') = showNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence t@(ProgWrite vs t')
  | S.size vs == 1 && S.member emptyPattern vs = traceToStringSequence t'
  | otherwise                                  = showNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence Stop = [showNTraceHead Plain Stop]
traceToStringSequence OutOfInputs = [showNTraceHead Plain OutOfInputs]

similar :: Trace' a -> Trace' b -> Bool
similar x y = inputs x == inputs y

inputs :: Trace' a -> [String]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
