{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTasks.Trace (
  Trace(..),
  OrdinaryTrace,
  GeneralizedTrace,
  MergeSet(..),
  NTrace(..),
  normalize,
  isCoveredBy,
  printGenNTraceInfo,
  inputsN,
  inputs,
  MatchResult(..),
  ppResult,
  ppTrace,
) where

import Prelude hiding (GT)

import Test.IOTasks.Pattern

import           Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.HughesPJClass (Pretty,Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP

type OrdinaryTrace = Trace String
type GeneralizedTrace = NTrace (MergeSet FixedPattern)

-- newtype wrapper for Set to control Semigroup/Monoid behavior
newtype MergeSet a = MkMergeSet { fromMergeSet :: Set a }
  deriving Show via Set a

instance (Ord a, Semigroup a) => Semigroup (MergeSet a) where
  MkMergeSet xs <> MkMergeSet ys = MkMergeSet $ S.map (uncurry (<>)) $ S.cartesianProduct xs ys

data Trace a
  = ProgRead String (Trace a)
  | ProgWrite a (Trace a)
  | Stop
 deriving (Eq, Show, Functor)

data NTrace a
  = ProgWriteReadN a String (NTrace a) -- !_ ?_ t
  | ProgReadN String (NTrace a) -- ?_ t
  | ProgWriteStopN a -- !_ stop
  | StopN -- stop
  deriving (Show, Eq, Functor)

instance Semigroup (Trace a) where
  t1 <> Stop = t1
  Stop <> t2 = t2
  (ProgRead v1 t1) <> t2 = ProgRead v1 $ t1 <> t2
  (ProgWrite v t1) <> t2 = ProgWrite v $ t1 <> t2

instance Monoid (Trace a) where
  mempty = Stop

instance Semigroup a => Semigroup (NTrace a) where
  t1 <> StopN = t1
  StopN <> t2 = t2
  (ProgWriteReadN vs v t1) <> t2 = ProgWriteReadN vs v $ t1 <> t2
  (ProgReadN v t1) <> t2 = ProgReadN v $ t1 <> t2
  (ProgWriteStopN vs1) <> (ProgWriteReadN vs2 v t2) = ProgWriteReadN (vs1 <> vs2) v t2
  (ProgWriteStopN vs) <> (ProgReadN v t2) = ProgWriteReadN vs v t2
  (ProgWriteStopN vs1) <> (ProgWriteStopN vs2) = ProgWriteStopN (vs1 <> vs2)

instance Semigroup a => Monoid (NTrace a) where
  mempty = StopN

normalize :: Semigroup a => Trace a -> NTrace a
normalize = go Nothing where
  go :: Semigroup a => Maybe a -> Trace a -> NTrace a
  go Nothing (ProgRead v t2) = ProgReadN v $ go Nothing t2
  go (Just p) (ProgRead v t2) = ProgWriteReadN p v $ go Nothing t2
  go Nothing (ProgWrite v t2) = go (Just v) t2
  go (Just p) (ProgWrite v t2) = go (Just $ p <> v) t2
  go Nothing Stop = StopN
  go (Just p) Stop = ProgWriteStopN p

data MatchResult
  = MatchSuccessfull
  | InputMismatch Doc
  | OutputMismatch Doc
  | AlignmentMismatch Doc
  | TerminationMismatch Doc
  deriving (Show, Eq)

ppResult :: MatchResult -> Doc
ppResult MatchSuccessfull = PP.text $ show MatchSuccessfull
ppResult (InputMismatch msg) = PP.hang (PP.text "InputMismatch:") 4 msg
ppResult (OutputMismatch msg) = PP.hang (PP.text "OutputMismatch:") 4 msg
ppResult (AlignmentMismatch msg) = PP.hang (PP.text "AlignmentMismatch:") 4 msg
ppResult (TerminationMismatch msg) = PP.hang (PP.text "TerminationMismatch:") 4 msg

{- HLINT ignore isCoveredBy-}
isCoveredBy :: OrdinaryTrace -> GeneralizedTrace -> MatchResult
isCoveredBy u v = isCoveredBy' (normalize u) v
  where
  isCoveredBy' :: NTrace String -> GeneralizedTrace -> MatchResult
  t1@(ProgReadN x t1') `isCoveredBy'` t2@(ProgReadN y t2') =
    if x == y
      then t1' `isCoveredBy'` t2'
      else InputMismatch $ reportExpectationMismatch t2 t1
  t1@(ProgWriteReadN vs1 v1 t1') `isCoveredBy'` (ProgWriteReadN (MkMergeSet vs2) v2 t2')
    | vs1 `isMatchesByOneOf` vs2 = isCoveredBy' (ProgReadN v1 t1') (ProgReadN v2 t2')
    | S.size vs2 == 1 && S.member emptyPattern vs2 = InputMismatch $ reportExpectationMismatch (ProgReadN v2 t2') t1
    | otherwise = OutputMismatch $ reportCoverageFailure vs1 vs2
  (ProgWriteStopN vs1) `isCoveredBy'` (ProgWriteStopN (MkMergeSet vs2))
    | vs1 `isMatchesByOneOf` vs2 = MatchSuccessfull
    | otherwise = OutputMismatch $ reportCoverageFailure vs1 vs2
  StopN `isCoveredBy'` StopN = MatchSuccessfull
  t1 `isCoveredBy'` (ProgWriteReadN (MkMergeSet vs2) v2 t2') | S.member emptyPattern vs2 = t1 `isCoveredBy'` ProgReadN v2 t2'
  t1 `isCoveredBy'` (ProgWriteStopN (MkMergeSet vs2)) | S.member emptyPattern vs2 = t1 `isCoveredBy'` StopN
  t1 `isCoveredBy'` StopN = TerminationMismatch (reportTerminationMismatch t1)
  t1 `isCoveredBy'` t2 = AlignmentMismatch (reportExpectationMismatch t2 t1)

isMatchesByOneOf :: String -> Set FixedPattern -> Bool
x `isMatchesByOneOf` xs = any (x `isContainedIn`) (S.toList xs)

reportCoverageFailure :: String -> Set FixedPattern -> Doc
reportCoverageFailure xs ys = PP.text "the value" PP.<+> PP.doubleQuotes (PP.text (fixLinebreaks xs)) PP.<+> PP.text "is not covered by" PP.<+> printSet ys
  where
    fixLinebreaks = foldr f ""
    f '\n' cs = '\\':'n':cs
    f c cs    = c:cs

printSet :: Pretty a => Set a -> Doc
printSet set = PP.braces . PP.hsep . PP.punctuate (PP.text ",") $ PP.pPrint <$> S.toList set

reportExpectationMismatch :: GeneralizedTrace -> NTrace String -> Doc
reportExpectationMismatch t1 t2 = ppNTraceHead Expected t1 PP.$$ ppTraceHead PP.text Got t2

reportTerminationMismatch :: NTrace a -> Doc
reportTerminationMismatch t = ppNTraceHead Expected StopN PP.$$ ppTraceHead (const $ PP.text "...") Got t

instance Pretty a => Pretty (Trace a) where
  pPrint = ppTrace (-1) -- TODO: handle this in a less hacky way

ppTrace :: Pretty a => Int -> Trace a -> Doc
ppTrace 0 (ProgRead _ _) = PP.text "..."
ppTrace n (ProgRead v t) = PP.hcat [PP.text "?",PP.pPrint v, PP.text " ", ppTrace (n-1) t]
ppTrace n (ProgWrite v t) = PP.hcat [PP.text "!", PP.pPrint v, PP.text " ", ppTrace n t]
ppTrace _ Stop = PP.text "stop"

instance Pretty GeneralizedTrace where
  pPrint = ppNTrace

ppTraceFF :: (NTrace a -> Doc) -> (a -> Doc) -> NTrace a -> Doc
ppTraceFF ff _   (ProgReadN v t)         = (PP.text "?" <> PP.text v) PP.<+> ff t
ppTraceFF ff sho (ProgWriteReadN vs v t) = (PP.text "!" <> sho vs) PP.<+> (PP.text "?" <> PP.text v) PP.<+> ff t
ppTraceFF _  sho (ProgWriteStopN vs)     = (PP.text "!" <> sho vs) PP.<+> PP.text "stop"
ppTraceFF _  _   StopN                   = PP.text "stop"

data ShowHeadType = Expected | Got | Plain

ppTraceHead :: (a -> Doc) -> ShowHeadType -> NTrace a -> Doc
ppTraceHead sho Expected t = PP.text "Expected: " <> ppTraceHead sho Plain t
ppTraceHead sho Got      t = PP.text "Got: " <> ppTraceHead sho Plain t
ppTraceHead sho Plain    t = ppTraceFF (const mempty) sho t

ppNTraceHead :: ShowHeadType -> GeneralizedTrace -> Doc
ppNTraceHead = ppTraceHead (printSet . fromMergeSet)

printGenNTraceInfo :: GeneralizedTrace -> Doc
printGenNTraceInfo t = inputSequence PP.$+$ generalizedRun
  where inputSequence = PP.hang (PP.text "Input sequence:") 4 $ PP.hsep $ PP.text . ('?' :) <$> inputsN t
        generalizedRun = PP.hang (PP.text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: GeneralizedTrace -> Doc
ppNTrace = PP.hsep . traceToStringSequence

traceToStringSequence :: GeneralizedTrace -> [Doc]
traceToStringSequence t@(ProgWriteReadN (MkMergeSet vs) _ t')
  | S.size vs == 1 && S.member emptyPattern vs = traceToStringSequence t'
  | otherwise                                  = ppNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence t@(ProgReadN _ t') = ppNTraceHead Plain t : traceToStringSequence t'
traceToStringSequence t@(ProgWriteStopN _) = [ppNTraceHead Plain t]
traceToStringSequence StopN = [ppNTraceHead Plain StopN]

inputsN :: NTrace a  -> [String]
inputsN (ProgReadN v t) = v : inputsN t
inputsN (ProgWriteReadN _ v t) = v : inputsN t
inputsN (ProgWriteStopN _) = []
inputsN StopN = []

inputs :: Trace a  -> [String]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
