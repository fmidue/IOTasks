{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Trace (
  Trace(..),
  OrdinaryTrace,
  GeneralizedTrace,
  MergeSet(..),
  NTrace(..),
  normalize,
  isCoveredBy,
  printGenNTraceInfo,
  inputsN,
  MatchResult(..),
  ppResult,
) where

import Prelude hiding (GT)

import Test.IOTest.Pattern

import           Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.HughesPJClass hiding ((<>))

type OrdinaryTrace = Trace String
type GeneralizedTrace = NTrace (MergeSet Pattern)

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
  deriving Show

ppResult :: MatchResult -> Doc
ppResult MatchSuccessfull = text $ show MatchSuccessfull
ppResult (InputMismatch msg) = hang (text "InputMismatch:") 4 msg
ppResult (OutputMismatch msg) = hang (text "OutputMismatch:") 4 msg
ppResult (AlignmentMismatch msg) = hang (text "AlignmentMismatch:") 4 msg

isCoveredBy :: OrdinaryTrace -> GeneralizedTrace -> MatchResult
isCoveredBy u v = isCoveredBy' (normalize u) v
  where
  isCoveredBy' :: NTrace String -> GeneralizedTrace -> MatchResult
  t1@(ProgReadN x t1') `isCoveredBy'` t2@(ProgReadN y t2') =
    if x == y
      then t1' `isCoveredBy'` t2'
      else InputMismatch $ reportExpectationMismatch t2 t1
  t1@(ProgWriteReadN vs1 v1 t1') `isCoveredBy'` (ProgWriteReadN (MkMergeSet vs2) v2 t2')
    | buildPattern vs1 `isSubPatternIn` vs2 = isCoveredBy' (ProgReadN v1 t1') (ProgReadN v2 t2')
    | S.size vs2 == 1 && S.member emptyPattern vs2 = InputMismatch $ reportExpectationMismatch (ProgReadN v2 t2') t1
    | otherwise = OutputMismatch $ reportCoverageFailure vs1 vs2
  (ProgWriteStopN vs1) `isCoveredBy'` (ProgWriteStopN (MkMergeSet vs2))
    | buildPattern vs1 `isSubPatternIn` vs2 = MatchSuccessfull
    | otherwise = OutputMismatch $ reportCoverageFailure vs1 vs2
  StopN `isCoveredBy'` StopN = MatchSuccessfull
  t1 `isCoveredBy'` (ProgWriteReadN (MkMergeSet vs2) v2 t2') | S.member emptyPattern vs2 = t1 `isCoveredBy'` ProgReadN v2 t2'
  t1 `isCoveredBy'` (ProgWriteStopN (MkMergeSet vs2)) | S.member emptyPattern vs2 = t1 `isCoveredBy'` StopN
  t1 `isCoveredBy'` t2 = AlignmentMismatch (reportExpectationMismatch t2 t1)

isSubPatternIn :: Pattern -> Set Pattern -> Bool
x `isSubPatternIn` y = all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) [x]
                          -- '           '        '- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

reportCoverageFailure :: String -> Set Pattern -> Doc
reportCoverageFailure xs ys = text "the value" <+> text xs <+> text "is not covered by" <+> printSet ys

printSet :: Pretty a => Set a -> Doc
printSet set = braces . hsep . punctuate (text ",") $ pPrint <$> S.toList set

reportExpectationMismatch :: GeneralizedTrace -> NTrace String -> Doc
reportExpectationMismatch t1 t2 = ppNTraceHead Expected t1 $$ ppTraceHead text Got t2
--
instance Pretty a => Pretty (Trace a) where
  pPrint (ProgRead v t) = hcat [text "?",pPrint v, text " ", pPrint t]
  pPrint (ProgWrite v t) = hcat [text "!", pPrint v, text " ", pPrint t]
  pPrint Stop = text "stop"

instance Pretty GeneralizedTrace where
  pPrint = ppNTrace

ppTraceFF :: (NTrace a -> Doc) -> (a -> Doc) -> NTrace a -> Doc
ppTraceFF ff _   (ProgReadN v t)         = (text "?" <> text v) <+> ff t
ppTraceFF ff sho (ProgWriteReadN vs v t) = (text "!" <> sho vs) <+> (text "?" <> text v) <+> ff t
ppTraceFF _  sho (ProgWriteStopN vs)     = (text "!" <> sho vs) <+> text "stop"
ppTraceFF _  _   StopN                   = text "stop"

data ShowHeadType = Expected | Got | Plain

ppTraceHead :: (a -> Doc) -> ShowHeadType -> NTrace a -> Doc
ppTraceHead sho Expected t = text "Expected: " <> ppTraceHead sho Plain t
ppTraceHead sho Got      t = text "Got: " <> ppTraceHead sho Plain t
ppTraceHead sho Plain    t = ppTraceFF (const empty) sho t

ppNTraceHead :: ShowHeadType -> GeneralizedTrace -> Doc
ppNTraceHead = ppTraceHead (printSet . fromMergeSet)

printGenNTraceInfo :: GeneralizedTrace -> Doc
printGenNTraceInfo t = inputSequence $+$ generalizedRun
  where inputSequence = hang (text "Input sequence:") 4 $ hsep $ (text . ('?' :)) <$> inputsN t
        generalizedRun = hang (text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: GeneralizedTrace -> Doc
ppNTrace = hsep . traceToStringSequence

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
