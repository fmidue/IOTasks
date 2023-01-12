{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module IOTasks.Trace (
  AbstractTrace, Trace, NTrace,
  ordinaryTrace, normalizedTrace,
  terminate, outOfInputs, progWrite, progRead,
  pattern Terminate, pattern OutOfInputs, pattern ProgWrite, pattern ProgRead, pattern ProgReadString,
  pattern NTerminate, pattern NOutOfInputs, pattern NProgWrite, pattern NProgRead, pattern NProgReadString,
  pPrintTrace, pPrintTraceSimple,
  inputSequence, isTerminating,
  inputSequenceN, isTerminatingN,

  OptFlag(..),
  MatchResult(..), pPrintMatchResult, pPrintMatchResultSimple,
  covers,
) where

import IOTasks.OutputPattern

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Function (fix)

import Text.PrettyPrint hiding ((<>))

import Data.List.NonEmpty (NonEmpty(..))
import Data.Functor.Classes (Show1, showsPrec1, Eq1, eq1)
import Text.Show.Deriving (deriveShow1)
import Data.Eq.Deriving (deriveEq1)

data OptFlag = Optional | Mandatory deriving (Eq, Ord, Show)

-- "polymorphic" unit type
data U a = U deriving Show
newtype I a = I a deriving Show

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

progWrite :: OptFlag -> Set (OutputPattern 'TraceP) -> AbstractTrace
progWrite o ts = AbstractTrace $ ProgWriteC o ts U :| []

terminate :: AbstractTrace
terminate = AbstractTrace $ TerminateC :| []

outOfInputs :: AbstractTrace
outOfInputs = AbstractTrace $ OutOfInputsC :| []

data MatchResult
  = MatchSuccessfull
  | InputMismatch NTrace NTrace
  | OutputMismatch NTrace NTrace
  | AlignmentMismatch NTrace (Maybe NTrace) NTrace
  | TerminationMismatch NTrace (Maybe NTrace) NTrace
  deriving (Eq,Show)

instance Semigroup MatchResult where
  MatchSuccessfull <> _ = MatchSuccessfull
  _ <> MatchSuccessfull = MatchSuccessfull
  r <> _ = r

addExpect :: NTrace -> MatchResult -> MatchResult
addExpect s' (AlignmentMismatch t _ s) = AlignmentMismatch t (Just s') s
addExpect s' (TerminationMismatch t _ s) = TerminationMismatch t (Just s') s
addExpect _ r = r

covers :: NTrace -> NTrace -> MatchResult
covers s@(NProgRead i t1) t@(NProgRead j t2)
  | i == j = t1 `covers` t2
  | otherwise = InputMismatch s t

covers s@(NProgWrite Mandatory is t1) t@(NProgWrite Mandatory js t2)
  | all (\j -> any (>: j) is) js = t1 `covers` t2
  | otherwise = OutputMismatch s t

covers s@(NProgWrite Optional is t1) t = (NProgWrite Mandatory is t1 `covers` t) <> addExpect s (t1 `covers` t)
covers s t@(NProgWrite Optional _ _) = OutputMismatch s t

covers NTerminate NTerminate = MatchSuccessfull
covers s@NTerminate t = TerminationMismatch s Nothing t

covers NOutOfInputs NOutOfInputs = MatchSuccessfull

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


reportMismatch :: Bool -> NTrace -> Maybe NTrace -> NTrace -> Doc
reportMismatch simple s s' t = vcat
  [ text "Expected:"
  , nest 2 (maybe mempty ((<+> text "or") . showTraceHead simple) s' <+> showTraceHead simple s)
  , text "Got:"
  , nest 2 (showTraceHead simple t)
  ]

reportOutputMismatch :: Bool -> NTrace -> NTrace -> Doc
reportOutputMismatch simple s t = showTraceHead simple t <+> text "is not covered by" <+> showTraceHead simple s

showTraceHead :: Bool -> NTrace -> Doc
showTraceHead simple = text . showTraceHead' simple (const "")

pPrintTrace :: NTrace -> String
pPrintTrace = fix (showTraceHead' False)

pPrintTraceSimple :: NTrace -> String
pPrintTraceSimple = fix (showTraceHead' True)

showTraceHead' :: Bool -> (NTrace -> String) -> NTrace -> String
showTraceHead' simple f (NProgRead x (NProgRead '\n' t)) = "?"++ [x] ++ (if simple then "" else "\\n") ++ addSpace (f t)
showTraceHead' simple f (NProgRead x (NProgRead c t)) = "?"++ x : tail (showTraceHead' simple f (NProgRead c t))
showTraceHead' _ f (NProgRead x t') = "?"++[x] ++ addSpace (f t')
showTraceHead' simple f (NProgWrite Optional ts t')
  | simple = "(!"++ (head $ printPatternSimple <$> Set.toList ts) ++ ")" ++ addSpace (f t') -- omit optional outputs in simplified version
  | otherwise  = "(!{"++ intercalate "," (printPattern <$> Set.toList ts) ++ "})" ++ addSpace (f t')
showTraceHead' simple f (NProgWrite Mandatory ts t')
  | simple = "!"++ (head $ printPatternSimple <$> Set.toList ts) ++ addSpace (f t')
  | otherwise = "!{"++ intercalate "," (printPattern <$> Set.toList ts) ++ "}" ++ addSpace (f t')
showTraceHead' _ _ NTerminate = "stop"
showTraceHead' _ _ NOutOfInputs = "?<unknown input>"

addSpace :: String -> String
addSpace "" = ""
addSpace s = ' ':s

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
  -- technically this might add an additional linebreak on the last line that might not be there in the Trace
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
  -- technically this might add an additional linebreak on the last line that might not be there in the Trace
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
