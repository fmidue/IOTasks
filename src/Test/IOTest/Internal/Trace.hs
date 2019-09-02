{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTest.Internal.Trace (
  NTrace,
  Trace,
  Trace'(..),
  TraceAction(..),
  normalize,
  isCoveredBy,
  printNTraceInfo,
  inputs,
  MatchResult(..),
  ppResult,
) where

import Test.IOTest.Internal.Pattern
import Test.IOTest.Utils

import           Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Data.Function (on)

newtype Trace' o = Trace { toList :: [TraceAction o] } deriving Show

instance Semigroup NTrace where
  Trace [] <> Trace t2 = Trace t2
  Trace t1 <> Trace t2 = Trace (init t1 <> (last t1 >* t2)) where
    ProgWrite v1 >* (ProgWrite v2 : t') =
      let v = S.map (uncurry (<>)) $ S.cartesianProduct v1 v2
      in (ProgWrite v : t')
    t >* t' = t : t'

instance Monoid NTrace where
  mempty = Trace mempty

instance Semigroup Trace where
  (<>) = Trace ... ((<>) `on` toList)

instance Monoid Trace where
  mempty = Trace mempty

data TraceAction o
  = ProgRead String
  | ProgWrite o
  | OutOfInputs
  deriving (Eq,Functor,Show)

type Trace = Trace' String
type NTrace = Trace' (Set LinearPattern)

normalize :: Trace -> Trace
normalize = Trace . go "" . toList where
  go p (ProgWrite v : t') = go (p <> v) t'
  go p (ProgRead v : t') = ProgWrite p : ProgRead v : go "" t'
  go p [] = [ProgWrite p]
  go p (OutOfInputs : _) = [ProgWrite p, OutOfInputs]

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

isCoveredBy :: Trace -> NTrace -> MatchResult
isCoveredBy (Trace u) (Trace v) = isCoveredBy' u v
  where
  t1@(ProgRead x : t1') `isCoveredBy'` t2@(ProgRead y : t2') =
    if x == y
      then t1' `isCoveredBy'` t2'
      else InputMismatch (reportExpectationMismatch (Trace t2) (Trace t1))
  t1@(ProgWrite v1 : t1') `isCoveredBy'` (ProgWrite v2 : t2')
    | buildPattern v1 `isSubPatternIn` v2 = t1' `isCoveredBy'` t2'
    | S.size v2 == 1 && S.member emptyPattern v2 = InputMismatch $ reportExpectationMismatch (Trace t2') (Trace t1)
    | otherwise = OutputMismatch $ reportCoverageFailure v1 v2
  (ProgWrite "" : t1') `isCoveredBy'` t2 = t1' `isCoveredBy'` t2
  [] `isCoveredBy'` [] = MatchSuccessfull
  t1 `isCoveredBy'` t2 = AlignmentMismatch (reportExpectationMismatch (Trace t2) (Trace t1))

isSubPatternIn :: LinearPattern -> Set LinearPattern -> Bool
x `isSubPatternIn` y = all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) [x]
                          -- '           '        '- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

reportCoverageFailure :: String -> Set LinearPattern -> Doc
reportCoverageFailure xs ys = text "the value" <+> text xs <+> text "is not covered by" <+> printSet ys

printSet :: Pretty a => Set a -> Doc
printSet set = braces . hsep . punctuate (text ",") $ pPrint <$> S.toList set

reportExpectationMismatch :: NTrace -> Trace -> Doc
reportExpectationMismatch t1 t2 = ppNTraceHead Expected t1 $$ ppTraceHead text Got t2

instance Pretty Trace where
  pPrint (Trace (ProgRead v : t)) = hcat [text "?",pPrint v, text " ", pPrint (Trace t)]
  pPrint (Trace (ProgWrite v : t)) = hcat [text "!", pPrint v, text " ", pPrint (Trace t)]
  pPrint (Trace []) = text "stop"
  pPrint (Trace (OutOfInputs : _)) = text "<out of inputs>"

instance Pretty NTrace where
  pPrint = ppNTrace

ppTraceFF :: (Trace' o -> Doc) -> (o -> Doc) -> Trace' o -> Doc
ppTraceFF ff _   (Trace (ProgRead v : t))   = (text "?" <> text v) <+> ff (Trace t)
ppTraceFF ff sho (Trace (ProgWrite v : t))  = (text "!" <> sho v) <+> ff (Trace t)
ppTraceFF _  _   (Trace [])               = text "stop"
ppTraceFF _  _   (Trace (OutOfInputs : _ )) = text "<out of inputs>"

data ShowHeadType = Expected | Got | Plain

ppTraceHead :: (o -> Doc) -> ShowHeadType -> Trace' o -> Doc
ppTraceHead sho Expected t = text "Expected: " <> ppTraceHead sho Plain t
ppTraceHead sho Got      t = text "Got: " <> ppTraceHead sho Plain t
ppTraceHead sho Plain    t = ppTraceFF (const empty) sho t

ppNTraceHead :: ShowHeadType -> NTrace -> Doc
ppNTraceHead = ppTraceHead printSet

printNTraceInfo :: NTrace -> Doc
printNTraceInfo t = inputSequence $+$ generalizedRun
  where inputSequence = hang (text "Input sequence:") 4 $ hsep $ (text . ('?' :)) <$> inputs t
        generalizedRun = hang (text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: NTrace -> Doc
ppNTrace = hsep . traceToStringSequence

traceToStringSequence :: NTrace -> [Doc]
traceToStringSequence t@(Trace (ProgRead _ : t')) = ppNTraceHead Plain t : traceToStringSequence (Trace t')
traceToStringSequence t@(Trace (ProgWrite vs : t'))
  | S.size vs == 1 && S.member emptyPattern vs = traceToStringSequence (Trace t')
  | otherwise                                  = ppNTraceHead Plain t : traceToStringSequence (Trace t')
traceToStringSequence (Trace []) = [ppNTraceHead Plain (Trace [])]
traceToStringSequence (Trace (OutOfInputs : _))  = [ppNTraceHead Plain (Trace [OutOfInputs])]

similar :: Trace' a -> Trace' b -> Bool
similar x y = inputs x == inputs y

inputs :: Trace' a -> [String]
inputs (Trace (ProgRead v : t)) = v : inputs (Trace t)
inputs (Trace (ProgWrite _ : t)) = inputs (Trace t)
inputs (Trace []) = []
inputs (Trace (OutOfInputs : _)) = []
