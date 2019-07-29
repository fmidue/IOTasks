{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.Internal.Trace (
  NTrace,
  Trace,
  Trace'(..),
  normalize,
  isCoveredBy,
  printNTraceInfo,
  inputs,
  MatchResult(..),
  ppResult,
) where

import Test.IOTest.Internal.Pattern

import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List
-- import           Data.Function ( fix )

-- import Text.PrettyPrint hiding ((<>))
import Text.PrettyPrint.HughesPJClass hiding ((<>))

data Trace' o
  = ProgRead String (Trace' o)
  | ProgWrite o (Trace' o)
  | Stop
  | OutOfInputs
  deriving (Eq,Functor,Show)

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
t1 `isCoveredBy` t2 = AlignmentMismatch (reportExpectationMismatch t2 t1)

isSubsetOf :: Set LinearPattern -> Set LinearPattern -> Bool
x `isSubsetOf` y = x == y || all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) (S.toList x)
                          -- '           '        '- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

reportCoverageFailure :: Set LinearPattern -> Set LinearPattern -> Doc
reportCoverageFailure xs ys = text "the set" <+> printSet xs <+> text "is not a subset of" <+> printSet ys

printSet :: Pretty a => Set a -> Doc
printSet set = braces . hsep . punctuate (text ",") $ pPrint <$> S.toList set

reportExpectationMismatch :: NTrace -> NTrace -> Doc
reportExpectationMismatch t1 t2 = ppNTraceHead Expected t1 $$ ppNTraceHead Got t2

instance Pretty Trace where
  pPrint (ProgRead v t) = hcat [text "?",pPrint v, text " ", pPrint t]
  pPrint (ProgWrite v t) = hcat [text "!", pPrint v, text " ", pPrint t]
  pPrint Stop = text "stop"
  pPrint OutOfInputs = text "<out of inputs>"

instance Pretty NTrace where
  pPrint = ppNTrace

ppNTraceFF :: (NTrace -> Doc) -> NTrace -> Doc
ppNTraceFF ff (ProgRead v t) = hcat [text "?", text v, text " ", ff t]
ppNTraceFF ff (ProgWrite v t) = text "!" <> printSet v <> ff t
ppNTraceFF _ Stop = text "stop"
ppNTraceFF _ OutOfInputs = text "<out of inputs>"

data ShowHeadType = Expected | Got | Plain

ppNTraceHead :: ShowHeadType -> NTrace -> Doc
ppNTraceHead Expected t = text "Expected: " <> ppNTraceHead Plain t
ppNTraceHead Got t = text "Got: " <> ppNTraceHead Plain t
ppNTraceHead Plain t = ppNTraceFF (const empty) t

printNTraceInfo :: NTrace -> Doc
printNTraceInfo t = inputSequence $+$ generalizedRun
  where inputSequence = hang (text "Input sequence:") 4 $ hsep $ (text . ('?' :)) <$> inputs t
        generalizedRun = hang (text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: NTrace -> Doc
ppNTrace = hcat . traceToStringSequence

traceToStringSequence :: NTrace -> [Doc]
traceToStringSequence t@(ProgRead _ t') = ppNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence t@(ProgWrite vs t')
  | S.size vs == 1 && S.member emptyPattern vs = traceToStringSequence t'
  | otherwise                                  = ppNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence Stop = [ppNTraceHead Plain Stop]
traceToStringSequence OutOfInputs = [ppNTraceHead Plain OutOfInputs]

similar :: Trace' a -> Trace' b -> Bool
similar x y = inputs x == inputs y

inputs :: Trace' a -> [String]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
