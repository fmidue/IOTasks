{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Test.IOTasks.Trace (
  AbstractTrace,
  OptFlag(..),
  progRead, progReadLine, progWrite,
  terminate, outOfInputs,
  Trace,
  ordinaryTrace,
  pattern ProgRead, pattern ProgReadString, pattern ProgWrite,
  pattern Terminate, pattern OutOfInputs,
  inputSequence, isTerminating,
  NTrace,
  normalizedTrace,
  pattern NProgRead, pattern NProgReadString, pattern NProgWrite,
  pattern NTerminate, pattern NOutOfInputs,
  inputSequenceN, isTerminatingN,
  showTrace, showTraceSimple,
  showTraceN, showTraceNSimple,

  showTrace', showTraceSimple',
  showTraceN', showTraceNSimple',

  covers,
  MatchResult,
  isSuccessfulMatch, isInputMismatch, isOutputMismatch, isAlignmentMismatch, isTerminationMismatch,
  pPrintMatchResult, pPrintMatchResultSimple,
) where

import Test.IOTasks.OutputPattern hiding (text)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function (fix)
import Data.Bifunctor (second)

import Text.PrettyPrint hiding ((<>))

import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor.Classes (Show1, showsPrec1, Eq1, eq1)
import Text.Show.Deriving (deriveShow1)
import Data.Eq.Deriving (deriveEq1)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

-- "polymorphic" unit type
data U a = U deriving (Eq,Show)
newtype I a = I a deriving (Eq, Show)

deriveShow1 ''U
deriveShow1 ''I

deriveEq1 ''U
deriveEq1 ''I

-- free semigroup over single trace actions
newtype AbstractTrace = AbstractTrace (NonEmpty (Trace' U))
  deriving Semigroup via (NonEmpty (Trace' U))
  deriving Show

foldMapSemi :: Semigroup m => (a -> m) -> NonEmpty a -> m
foldMapSemi f (x :| []) = f x
foldMapSemi f (x :| (y:xs)) = f x <> foldMapSemi f (y :| xs)

concreteTrace :: Semigroup t => (Trace' I -> t) -> AbstractTrace -> t
concreteTrace c (AbstractTrace xs) = foldMapSemi (c . f) xs where
  f :: Trace' U -> Trace' I
  f (ProgReadC c U) = ProgReadC c $ I TerminateC
  f (ProgWriteC opt os U) = ProgWriteC opt os $ I TerminateC
  f TerminateC = TerminateC
  f OutOfInputsC = OutOfInputsC

ordinaryTrace :: AbstractTrace -> Trace
ordinaryTrace = concreteTrace Trace

normalizedTrace :: AbstractTrace -> NTrace
normalizedTrace = concreteTrace NTrace

instance Semigroup Trace where
  ProgRead c t <> t' = ProgRead c $ t <> t'
  ProgWrite o ts t <> t' = ProgWrite o ts $ t <> t'
  Terminate <> t' = t'
  OutOfInputs <> _ = OutOfInputs

instance Semigroup NTrace where
  NProgRead c t <> t' = NProgRead c $ t <> t'
  NProgWrite o1 ts1 t <> NProgWrite o2 ts2 t'
    = NProgWrite
      (max o1 o2)
      (Set.unions $
        [ ts1 | o2 == Optional ] ++
        [ ts2 | o1 == Optional ] ++
        [Set.map (uncurry (<>)) $ Set.cartesianProduct ts1 ts2]) t
      <> t'
  NProgWrite o ts t <> t' = NProgWrite o ts $ t <> t'
  NTerminate <> t' = t'
  NOutOfInputs <> _ = NOutOfInputs


newtype Trace = Trace (Trace' I) deriving (Eq, Show)
newtype NTrace = NTrace (Trace' I) deriving (Eq, Show)

data Trace' f
  = ProgReadC Char (f (Trace' f))
  | ProgWriteC OptFlag (Set (OutputPattern 'TraceP)) (f (Trace' f))
  | TerminateC
  | OutOfInputsC

instance Eq1 f => Eq (Trace' f) where
  ProgReadC c1 t1 == ProgReadC c2 t2 = c1 == c2 && t1 `eq1` t2
  ProgWriteC o1 os1 t1 == ProgWriteC o2 os2 t2 = o1 == o2 && os1 == os2 && t1 `eq1` t2
  OutOfInputsC == OutOfInputsC = True
  TerminateC == TerminateC = True
  _ == _ = False

instance Show1 f => Show (Trace' f) where
  showsPrec prec (ProgReadC c t) = showString "ProgReadC " . shows c . showChar ' ' . showsPrec1 prec t
  showsPrec prec (ProgWriteC opt os t) = showString "ProgWriteC " . shows opt . showChar ' ' . shows os . showChar ' ' . showsPrec1 prec t
  showsPrec _ TerminateC = showString "TerminateC"
  showsPrec _ OutOfInputsC = showString "OutOfInputsC"

progRead :: Char -> AbstractTrace
progRead c = AbstractTrace $ ProgReadC c U :| []

progReadLine :: String -> AbstractTrace
progReadLine "" = error "progReadString: empty string"
progReadLine (c:cs) = AbstractTrace $ ProgReadC c U :| map (`ProgReadC` U) (cs++['\n'])

progWrite :: OptFlag -> Set (OutputPattern 'TraceP) -> AbstractTrace
progWrite o ts = AbstractTrace $ ProgWriteC o ts U :| []

terminate :: AbstractTrace
terminate = AbstractTrace $ TerminateC :| []

outOfInputs :: AbstractTrace
outOfInputs = AbstractTrace $ OutOfInputsC :| []

data MatchResult
  = MatchSuccessful
  | InputMismatch NTrace NTrace
  | OutputMismatch NTrace NTrace
  | AlignmentMismatch NTrace NTrace
  | TerminationMismatch NTrace (Maybe NTrace) NTrace
  deriving (Eq,Show)

isSuccessfulMatch :: MatchResult -> Bool
isSuccessfulMatch MatchSuccessful = True
isSuccessfulMatch _ = False

isInputMismatch :: MatchResult -> Bool
isInputMismatch InputMismatch{} = True
isInputMismatch _ = False

isOutputMismatch :: MatchResult -> Bool
isOutputMismatch OutputMismatch{} = True
isOutputMismatch _ = False

isAlignmentMismatch :: MatchResult -> Bool
isAlignmentMismatch AlignmentMismatch{} = True
isAlignmentMismatch _ = False

isTerminationMismatch :: MatchResult -> Bool
isTerminationMismatch TerminationMismatch{} = True
isTerminationMismatch _ = False

instance Semigroup MatchResult where
  a <> b = case choose a b of
    Left x -> x
    Right x -> x

choose :: MatchResult -> MatchResult -> Either MatchResult MatchResult
choose MatchSuccessful _ = Left MatchSuccessful
choose _ MatchSuccessful = Right MatchSuccessful
choose r _ = Left r

addExpect :: NTrace -> (MatchResult,HasConsumed) -> (MatchResult,HasConsumed)
addExpect s' (TerminationMismatch t Nothing s,False) = (TerminationMismatch t (Just s') s,False)
addExpect _ r = r

type HasConsumed = Bool

covers :: NTrace -> NTrace -> MatchResult
covers s t = fst $ covers' s t
  where
    -- HasConsume tracks if the result has consumed at least one step from t
    covers' :: NTrace -> NTrace -> (MatchResult, HasConsumed)
    covers' s@(NProgRead i t1) t@(NProgRead j t2)
      | i == j = hasConsumed $ t1 `covers'` t2
      | otherwise = noConsume $ InputMismatch s t

    covers' s@(NProgWrite Mandatory is t1) t@(NProgWrite Mandatory js t2)
      | all (\j -> any (>: j) is) js = hasConsumed $ t1 `covers'` t2
      | otherwise = noConsume $ OutputMismatch s t

    covers' (NProgWrite Optional is t1@NProgRead{}) t@(NProgWrite{}) =
      -- avoid the alignment mismatch that would result from trying to skip the optional output
      NProgWrite Mandatory is t1 `covers'` t
    covers' s@(NProgWrite Optional is t1) t =
      let
        (r1,c1) = addExpect s (t1 `covers'` t) -- cannot produce alignment mismatch!
        (r2,c2) = (NProgWrite Mandatory is t1 `covers'` t)
      in case choose r1 r2 of -- This is r1 <> r2 + bookkeeping for the consumption status
        Left _ -> (r1,c1)
        Right _ -> (r2,c2)

    covers' s t@(NProgWrite Optional _ _) = noConsume $ OutputMismatch s t

    covers' NTerminate NTerminate = (MatchSuccessful,True)
    covers' s@NTerminate t = noConsume $ TerminationMismatch s Nothing t

    covers' NOutOfInputs NOutOfInputs = (MatchSuccessful,True)

    covers' s t = noConsume $ AlignmentMismatch s t

noConsume :: MatchResult -> (MatchResult,HasConsumed)
noConsume = (,False)

hasConsumed :: (MatchResult,HasConsumed) -> (MatchResult,HasConsumed)
hasConsumed = second (const True)

instance Pretty MatchResult where
  pPrint = pPrintMatchResult (<+>)

instance Pretty NTrace where
  pPrint = showTraceN

instance Pretty Trace where
  pPrint = showTrace

pPrintMatchResult :: (Doc -> Doc -> Doc) -> MatchResult -> Doc
pPrintMatchResult = pPrintMatchResult' False

pPrintMatchResultSimple :: (Doc -> Doc -> Doc) -> MatchResult -> Doc
pPrintMatchResultSimple = pPrintMatchResult' True

pPrintMatchResult' :: Bool -> (Doc -> Doc -> Doc) -> MatchResult -> Doc
pPrintMatchResult' _ _ MatchSuccessful = text "MatchSuccessful"
pPrintMatchResult' simple f (InputMismatch s t) = text "InputMismatch:" $$ nest 2 (reportMismatch simple f s Nothing t)
pPrintMatchResult' simple f (OutputMismatch s t) = text "OutputMismatch:" $$ nest 2 (reportOutputMismatch simple f s t)
pPrintMatchResult' simple f (AlignmentMismatch s t) = text "AlignmentMismatch:" $$ nest 2 (reportMismatch simple f s Nothing t)
pPrintMatchResult' simple f (TerminationMismatch s s' t) = text "TerminationMismatch:" $$ nest 2 (reportMismatch simple f s s' t)

reportMismatch :: Bool -> (Doc -> Doc -> Doc) -> NTrace -> Maybe NTrace -> NTrace -> Doc
reportMismatch simple f s s' t = vcat
  [ text "Expected:"
  , nest 2 (maybe mempty ((`f` text "or") . pPrintTraceNHead simple) s' <+> pPrintTraceNHead simple s)
  , text "Got:"
  , nest 2 (pPrintTraceNHead simple t)
  ]

reportOutputMismatch :: Bool -> (Doc -> Doc -> Doc) -> NTrace -> NTrace -> Doc
reportOutputMismatch simple f s t = pPrintTraceNHead simple t `f` text "is not covered by" `f` pPrintTraceNHead simple s

pPrintTraceNHead :: Bool ->  NTrace -> Doc
pPrintTraceNHead simple (NTrace t) = showConcreteTraceHead simple (<+>) (const $ text "") t

showTraceN :: NTrace -> Doc
showTraceN = showTraceN' (<+>)

-- | like 'showTraceN' but trace steps are combined with the user supplied function
showTraceN' :: (Doc -> Doc -> Doc) -> NTrace -> Doc
showTraceN' op (NTrace t) = showDeep False op t

showTraceNSimple :: NTrace -> Doc
showTraceNSimple = showTraceNSimple' (<+>)

-- | like 'showTraceNSimple' but trace steps are combined with the user supplied function
showTraceNSimple' :: (Doc -> Doc -> Doc) -> NTrace -> Doc
showTraceNSimple' op (NTrace t) = showDeep True op t

showTrace :: Trace -> Doc
showTrace = showTrace' (<+>)

-- | like 'showTrace' but trace steps are combined with the user supplied function
showTrace' :: (Doc -> Doc -> Doc) -> Trace -> Doc
showTrace' op (Trace t) = showDeep False op t

showTraceSimple :: Trace -> Doc
showTraceSimple = showTraceSimple' (<+>)

-- | like 'showTraceSimple' but trace steps are combined with the user supplied function
showTraceSimple' :: (Doc -> Doc -> Doc) -> Trace -> Doc
showTraceSimple' op (Trace t) = showDeep True op t

showDeep :: Bool -> (Doc -> Doc -> Doc) -> Trace' I -> Doc
showDeep simple f = fix (showConcreteTraceHead simple f)

showConcreteTraceHead :: Bool -> (Doc -> Doc -> Doc) -> (Trace' I -> Doc) -> Trace' I -> Doc
showConcreteTraceHead simple op f (ProgWriteC Optional ts (I t'))
  | simple = f t' -- omit optional outputs in simplified version
  | otherwise  = ("(!{"<> hcat (punctuate "," (text . showPattern <$> Set.toList ts)) <> "})") `op` f t'
showConcreteTraceHead simple op f (ProgWriteC Mandatory ts (I t'))
  | simple = ("!\""<> (head $ text . showPatternSimple <$> Set.toList ts) <> "\"") `op` f t'
  | otherwise = ("!{"<> hcat (punctuate "," (text . showPattern <$> Set.toList ts)) <> "}" ) `op` f t'
showConcreteTraceHead _ _ _ TerminateC = "stop"
showConcreteTraceHead _ _ _ OutOfInputsC = "?<unknown input>"
showConcreteTraceHead simple op f (ProgReadC x t') = showConcreteTraceRead simple f (ProgReadC x t') ""
  where
    showConcreteTraceRead :: Bool -> (Trace' I -> Doc) -> Trace' I -> String -> Doc
    showConcreteTraceRead simple f (ProgReadC '\n' (I t)) s = ("?" <> text (reverse s) <> (if simple then "" else "\\n")) `op` f t
    showConcreteTraceRead simple f (ProgReadC x (I t)) s = showConcreteTraceRead simple f t (x:s)
    showConcreteTraceRead simple f t "" = showConcreteTraceHead simple op f t
    showConcreteTraceRead _ f t s = ("?" <> text s) `op` f t

isTerminating :: Trace -> Bool
isTerminating (ProgRead _ t) = isTerminating t
isTerminating (ProgWrite _ _ t) = isTerminating t
isTerminating Terminate = True
isTerminating OutOfInputs = False

isTerminatingN :: NTrace -> Bool
isTerminatingN (NProgRead _ t) = isTerminatingN t
isTerminatingN (NProgWrite _ _ t) = isTerminatingN t
isTerminatingN NTerminate = True
isTerminatingN NOutOfInputs = False

inputSequence :: Trace -> [String]
inputSequence = go "" where
  go cs (ProgRead '\n' t) = reverse cs : go "" t
  go cs (ProgRead c t) = go (c:cs) t
  go "" (ProgWrite _ _ t) = go "" t
  go cs (ProgWrite _ _ t) = reverse cs : go "" t
  -- technically this might add an additional line break on the last line that might not be there in the Trace
  go "" Terminate = []
  go cs Terminate = [reverse cs]
  go "" OutOfInputs = []
  go cs OutOfInputs = [reverse cs]

inputSequenceN :: NTrace -> [String]
inputSequenceN = go "" where
  go cs (NProgRead '\n' t) = reverse cs : go "" t
  go cs (NProgRead c t) = go (c:cs) t
  go "" (NProgWrite _ _ t) = go "" t
  go cs (NProgWrite _ _ t) = reverse cs : go "" t
  -- technically this might add an additional line break on the last line that might not be there in the Trace
  go "" NTerminate = []
  go cs NTerminate = [reverse cs]
  go "" NOutOfInputs = []
  go cs NOutOfInputs = [reverse cs]

nextInput :: Trace -> Maybe (String,Trace)
nextInput (ProgRead '\n' t) = Just ("\n",t)
nextInput (ProgRead c t) = case nextInput t of
  Just (s,t) -> Just (c:s,t)
  Nothing -> Just ([c],t)
nextInput _ = Nothing

nextInputN :: NTrace -> Maybe (String,NTrace)
nextInputN (NProgRead '\n' t) = Just ("\n",t)
nextInputN (NProgRead c t) = case nextInputN t of
  Just (s,t) -> Just (c:s,t)
  Nothing -> Just ([c],t)
nextInputN _ = Nothing

-- Trace patterns
pattern Terminate :: Trace
pattern Terminate = Trace TerminateC

pattern OutOfInputs :: Trace
pattern OutOfInputs = Trace OutOfInputsC

{-# COMPLETE ProgReadString, ProgWrite, Terminate, OutOfInputs #-}
pattern ProgReadString :: String -> Trace -> Trace
pattern ProgReadString s t <- (nextInput -> Just (s,t)) where
  ProgReadString s t = foldr ProgRead t s

{-# COMPLETE ProgRead, ProgWrite, Terminate, OutOfInputs #-}
pattern ProgRead :: Char -> Trace -> Trace
pattern ProgRead c t <- Trace (ProgReadC c (I (Trace -> t))) where
  ProgRead c (Trace t) = Trace $ ProgReadC c $ I t

pattern ProgWrite :: OptFlag -> Set (OutputPattern 'TraceP) -> Trace -> Trace
pattern ProgWrite opt os t <- Trace (ProgWriteC opt os (I (Trace -> t))) where
  ProgWrite opt os (Trace t) = Trace $ ProgWriteC opt os $ I t

-- NTrace patterns
pattern NTerminate :: NTrace
pattern NTerminate = NTrace TerminateC

pattern NOutOfInputs :: NTrace
pattern NOutOfInputs = NTrace OutOfInputsC

{-# COMPLETE NProgReadString, NProgWrite, NTerminate, NOutOfInputs #-}
pattern NProgReadString :: String -> NTrace -> NTrace
pattern NProgReadString s t <- (nextInputN -> Just (s,t)) where
  NProgReadString s t = foldr NProgRead t s

{-# COMPLETE NProgRead, NProgWrite, NTerminate, NOutOfInputs #-}
pattern NProgRead :: Char -> NTrace -> NTrace
pattern NProgRead c t <- NTrace (ProgReadC c (I (NTrace -> t))) where
  NProgRead c (NTrace t) = NTrace $ ProgReadC c $ I t

pattern NProgWrite :: OptFlag -> Set (OutputPattern 'TraceP) -> NTrace -> NTrace
pattern NProgWrite opt os t <- NTrace (ProgWriteC opt os (I (NTrace -> t))) where
  NProgWrite opt os (NTrace t) = NTrace $ ProgWriteC opt os $ I t
