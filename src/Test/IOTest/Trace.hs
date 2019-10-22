{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Trace (
  OrdinaryTrace,
  progRead,
  progWrite,
  stop,
  outOfInputs,
  GeneralizedNTrace,
  progReadN,
  progWriteN,
  normalizeO,
  isCoveredBy,
  printGenNTraceInfo,
  inputsG,
  MatchResult(..),
  ppResult,
) where

import Prelude hiding (GT)

import Test.IOTest.Pattern
import Test.QuickCheck

import           Data.Set (Set)
import qualified Data.Set as S

import Text.PrettyPrint.HughesPJClass hiding ((<>))

newtype OrdinaryTrace = OT (Trace String)
  deriving (Eq, Show)
  deriving (Semigroup, Monoid, Pretty) via Trace String
newtype OrdinaryNTrace = ONT (NTrace String)
  deriving (Eq, Show)
newtype GeneralizedTrace = GT (Trace (Set Pattern))
  deriving (Eq, Show)
  deriving (Semigroup, Monoid) via Trace (Set Pattern)
newtype GeneralizedNTrace = GNT (NTrace (Set Pattern))
  deriving (Eq, Show)

data Trace a
 = ProgRead String (Trace a)
 | ProgWrite a (Trace a)
 | Stop
 | OutOfInputs
 deriving (Eq, Show, Functor)

instance Semigroup (Trace a) where
  (ProgRead v t1) <> t2 = ProgRead v $ t1 <> t2
  (ProgWrite v t1) <> t2 = ProgWrite v $ t1 <> t2
  Stop <> t2 = t2
  OutOfInputs <> _ = OutOfInputs

instance Monoid (Trace a) where
  mempty = Stop

progRead :: String -> OrdinaryTrace
progRead v = OT $ ProgRead v Stop

progWrite :: String -> OrdinaryTrace
progWrite v = OT $ ProgWrite v Stop

stop :: OrdinaryTrace
stop = OT mempty

outOfInputs :: OrdinaryTrace
outOfInputs = OT OutOfInputs

data NTrace a
  = NonWrite (RNTrace a)
  | ProgWriteN a (RNTrace a)
  deriving (Eq, Show, Functor)

data RNTrace a
  = ProgReadN String (NTrace a)
  | StopN
  | OutOfInputsN
  deriving (Eq, Show, Functor)

instance Semigroup GeneralizedNTrace where
  (GNT (ProgWriteN v t1)) <> (GNT (NonWrite t2)) = GNT $ ProgWriteN v (combineRN t1 t2) --t1 <> t2
  (GNT (ProgWriteN v1 StopN)) <> (GNT (ProgWriteN v2 t2)) = GNT $ ProgWriteN (concatPatterns v1 v2) t2
  (GNT (ProgWriteN v1 (ProgReadN v11 t1))) <> (GNT (ProgWriteN v2 t2)) = GNT $ ProgWriteN v1 $ ProgReadN v11 t' where (GNT t') = GNT t1 <> (GNT $ ProgWriteN v2 t2)
  (GNT (ProgWriteN v1 OutOfInputsN)) <> (GNT (ProgWriteN _ _)) = GNT $ ProgWriteN v1 OutOfInputsN
  (GNT (NonWrite (ProgReadN v t1))) <> t2 = GNT $ NonWrite $ ProgReadN v t' where (GNT t') = GNT t1 <> t2
  (GNT (NonWrite StopN)) <> t2 = t2
  (GNT (NonWrite OutOfInputsN)) <> _ = GNT $ NonWrite OutOfInputsN

combineRN :: RNTrace (Set Pattern) -> RNTrace (Set Pattern) -> RNTrace (Set Pattern)
combineRN (ProgReadN v t1) t2 = ProgReadN v t where (GNT t) = GNT t1 <> (GNT $ NonWrite t2)
combineRN StopN t2 = t2
combineRN OutOfInputsN _ = OutOfInputsN

concatPatterns :: Set Pattern -> Set Pattern -> Set Pattern
concatPatterns xs ys = S.map (uncurry (<>)) $ S.cartesianProduct xs ys

instance Monoid GeneralizedNTrace where
  mempty = GNT $ NonWrite StopN

progReadN :: String -> GeneralizedNTrace
progReadN v = GNT $ NonWrite $ ProgReadN v $ NonWrite StopN

progWriteN :: Set Pattern -> GeneralizedNTrace
progWriteN v = GNT $ ProgWriteN v StopN

stopN :: GeneralizedNTrace
stopN = GNT $ NonWrite StopN

outOfInputsN :: GeneralizedNTrace
outOfInputsN = GNT $ NonWrite OutOfInputsN

normalizeO :: OrdinaryTrace -> OrdinaryNTrace
normalizeO = go Nothing where
  go :: Maybe String -> OrdinaryTrace -> OrdinaryNTrace
  go (Just p) (OT (ProgWrite v t')) = go (Just $ p <> v) (OT t')
  go Nothing (OT (ProgWrite v t')) = go (Just v) (OT t')
  go Nothing (OT (ProgRead v t')) = ONT $ NonWrite $ ProgReadN v t'' where (ONT t'') = go Nothing (OT t')
  go (Just p) (OT (ProgRead v t')) = ONT $ ProgWriteN p $ ProgReadN v t'' where (ONT t'') = go Nothing (OT t')
  go Nothing (OT Stop) = ONT $ NonWrite StopN
  go (Just p) (OT Stop) = ONT $ ProgWriteN p StopN
  go Nothing (OT OutOfInputs) = ONT $ NonWrite OutOfInputsN
  go (Just p) (OT OutOfInputs) = ONT $ ProgWriteN p OutOfInputsN

normalizeG :: GeneralizedTrace -> GeneralizedNTrace
normalizeG = go Nothing where
  go :: Maybe (Set Pattern) -> GeneralizedTrace -> GeneralizedNTrace
  go Nothing (GT (ProgWrite v t')) = go (Just v) (GT t')
  go (Just p) (GT (ProgWrite v t'))
    | S.null p || S.size p == 1 && S.member emptyPattern p = go (Just v) (GT t')
    | otherwise = go (Just $ S.fromList [ x <> y | x <- S.toList p, y <- S.toList v ]) (GT t')
  go Nothing (GT (ProgRead v t')) = GNT $ NonWrite $ ProgReadN v t'' where (GNT t'') = go Nothing (GT t')
  go (Just p) (GT (ProgRead v t'))
    | S.null p || S.size p == 1 && S.member emptyPattern p = GNT $ NonWrite $ ProgReadN v t''
    | otherwise = GNT $ ProgWriteN p $ ProgReadN v t''
    where (GNT t'') = go Nothing (GT t')
  go Nothing (GT Stop) = GNT $ NonWrite StopN
  go (Just p) (GT Stop)
    | S.null p || S.size p == 1 && S.member emptyPattern p =  GNT $ NonWrite StopN
    | otherwise = GNT $ ProgWriteN p StopN
  go Nothing (GT OutOfInputs) = GNT $ NonWrite OutOfInputsN
  go (Just p) (GT OutOfInputs)
    | S.null p || S.size p == 1 && S.member emptyPattern p = GNT $ NonWrite OutOfInputsN
    | otherwise = GNT $ ProgWriteN p OutOfInputsN

generalized :: OrdinaryTrace -> GeneralizedTrace
generalized (OT t) = GT $ fmap (S.singleton . buildPattern) t

generalizedN :: OrdinaryNTrace -> GeneralizedNTrace
generalizedN (ONT t) = GNT $ fmap (S.singleton . buildPattern) t

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

isCoveredBy :: OrdinaryNTrace -> GeneralizedNTrace -> MatchResult
isCoveredBy (ONT u) (GNT v) = isCoveredBy' u v
  where
  -- isCoveredBy' :: OrdinaryNTrace -> GeneralizedNTrace -> MatchResult
  t1@(NonWrite (ProgReadN x t1')) `isCoveredBy'` t2@(NonWrite (ProgReadN y t2')) =
    if x == y
      then t1' `isCoveredBy'` t2'
      else InputMismatch $ reportExpectationMismatch (GNT t2) (ONT t1)
  t1@(ProgWriteN v1 t1') `isCoveredBy'` (ProgWriteN v2 t2')
    | buildPattern v1 `isSubPatternIn` v2 = isCoveredBy' (NonWrite t1') (NonWrite t2')
    | S.size v2 == 1 && S.member emptyPattern v2 = InputMismatch $ reportExpectationMismatch (GNT $ NonWrite t2') (ONT t1)
    | otherwise = OutputMismatch $ reportCoverageFailure v1 v2
  -- (ProgWriteN "" t1') `isCoveredBy'` t2 = NonWrite t1' `isCoveredBy'` t2
  (NonWrite StopN) `isCoveredBy'` NonWrite StopN = MatchSuccessfull
  t1 `isCoveredBy'` (ProgWriteN v2 t2') | S.member emptyPattern v2 = t1 `isCoveredBy'` NonWrite t2'
  t1 `isCoveredBy'` t2 = AlignmentMismatch (reportExpectationMismatch (GNT t2) (ONT t1))

isSubPatternIn :: Pattern -> Set Pattern -> Bool
x `isSubPatternIn` y = all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) [x]
                          -- '           '        '- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

reportCoverageFailure :: String -> Set Pattern -> Doc
reportCoverageFailure xs ys = text "the value" <+> text xs <+> text "is not covered by" <+> printSet ys

printSet :: Pretty a => Set a -> Doc
printSet set = braces . hsep . punctuate (text ",") $ pPrint <$> S.toList set

reportExpectationMismatch :: GeneralizedNTrace -> OrdinaryNTrace -> Doc
reportExpectationMismatch t1 (ONT t2) = ppNTraceHead Expected t1 $$ ppTraceHead text Got t2

instance Pretty a => Pretty (Trace a) where
  pPrint (ProgRead v t) = hcat [text "?",pPrint v, text " ", pPrint t]
  pPrint (ProgWrite v t) = hcat [text "!", pPrint v, text " ", pPrint t]
  pPrint Stop = text "stop"
  pPrint OutOfInputs = text "<out of inputs>"

instance Pretty GeneralizedNTrace where
  pPrint = ppNTrace
--
ppTraceFF :: (NTrace a -> Doc) -> (a -> Doc) -> NTrace a -> Doc
ppTraceFF ff sho (ProgWriteN v t)          = (text "!" <> sho v) <+> ff (NonWrite t)
ppTraceFF ff _   (NonWrite (ProgReadN v t)) = (text "?" <> text v) <+> ff t
ppTraceFF _  _   (NonWrite StopN)           = text "stop"
ppTraceFF _  _   (NonWrite OutOfInputsN)    = text "<out of inputs>"

data ShowHeadType = Expected | Got | Plain

ppTraceHead :: (a -> Doc) -> ShowHeadType -> NTrace a -> Doc
ppTraceHead sho Expected t = text "Expected: " <> ppTraceHead sho Plain t
ppTraceHead sho Got      t = text "Got: " <> ppTraceHead sho Plain t
ppTraceHead sho Plain    t = ppTraceFF (const empty) sho t

ppNTraceHead :: ShowHeadType -> GeneralizedNTrace -> Doc
ppNTraceHead ty (GNT t) = ppTraceHead printSet ty t

printGenNTraceInfo :: GeneralizedNTrace -> Doc
printGenNTraceInfo t@(GNT t') = inputSequence $+$ generalizedRun
  where inputSequence = hang (text "Input sequence:") 4 $ hsep $ (text . ('?' :)) <$> inputsG (GNT t')
        generalizedRun = hang (text "Expected run (generalized):") 4 $ ppNTrace t

-- ommit occurences of !{}
ppNTrace :: GeneralizedNTrace -> Doc
ppNTrace = hsep . traceToStringSequence

traceToStringSequence :: GeneralizedNTrace -> [Doc]
traceToStringSequence t@(GNT (ProgWriteN vs t'))
  | S.size vs == 1 && S.member emptyPattern vs = traceToStringSequence (GNT $ NonWrite t')
  | otherwise                                  = ppNTraceHead Plain t : traceToStringSequence (GNT $ NonWrite t')
traceToStringSequence t@(GNT (NonWrite (ProgReadN _ t'))) = ppNTraceHead Plain t : traceToStringSequence (GNT t')
traceToStringSequence (GNT (NonWrite StopN)) = [ppNTraceHead Plain (GNT $ NonWrite StopN)]
traceToStringSequence (GNT (NonWrite OutOfInputsN))  = [ppNTraceHead Plain (GNT $ NonWrite OutOfInputsN)]

-- similar :: Trace' a -> Trace' b -> Bool
-- similar x y = inputs x == inputs y

inputsG :: GeneralizedNTrace -> [String]
inputsG (GNT (ProgWriteN _ t)) = inputsG (GNT $ NonWrite t)
inputsG (GNT (NonWrite (ProgReadN v t))) = v : inputsG (GNT t)
inputsG (GNT (NonWrite StopN)) = []
inputsG (GNT (NonWrite OutOfInputsN)) = []

-- ------------- --
-- ------------- --

prop_commute :: OrdinaryTrace -> Bool
prop_commute (OT t) = generalizedN (normalizeO (OT t)) == normalizeG (generalized (OT t))

instance Arbitrary OrdinaryTrace where
  arbitrary = OT <$> sized (\n ->
    if n == 0
      then return Stop
      else
        let t' = scale (subtract 1) (arbitrary @OrdinaryTrace)
        in oneof [ pure Stop
                 , (\v (OT t) -> ProgRead v t) <$> arbitrary <*> t'
                 , (\v (OT t) -> ProgWrite v t) <$> arbitrary <*> t'
                 ])
  shrink (OT Stop) = []
  shrink (OT (ProgRead v t')) = [OT Stop, OT t'] ++ [OT $ ProgRead v t'' | (OT t'') <- shrink (OT t') ]
  shrink (OT (ProgWrite v t')) = [OT Stop, OT t'] ++ [OT $ ProgWrite v t'' | (OT t'') <- shrink (OT t') ]
  shrink (OT OutOfInputs) = error "OutOfInputs should not have been generated"

test :: IO ()
test = quickCheck $ prop_commute
