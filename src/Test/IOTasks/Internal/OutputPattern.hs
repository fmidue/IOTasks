{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTasks.Internal.OutputPattern (
  PatternKind (..),
  OutputPattern(..),
  convert,
  wildcard, nonEmptyWildcard, text, resultOf,
  decoratedResultOf,
  valueTerms,
  showPattern, showPatternSimple,
  evalPattern,
  AddLinebreaks, evalPatternSet, evalPatternSet',
  (>:),
  ) where

import Prelude hiding (all)

import Test.IOTasks.Overflow
import Test.IOTasks.Internal.Term
import Test.IOTasks.ValueMap

import Control.Monad (void)

import Data.Bifunctor (second)
import Data.Char (isPrint, showLitChar, isSpace)
import Data.Either (isRight)
import Data.GADT.Compare
import Data.List (all)
import Data.Set as Set (Set)
import qualified Data.Set as Set

import Text.Parsec
import Type.Reflection

data OutputPattern (k :: PatternKind) where
  Wildcard :: WildcardType -> OutputPattern k
  Text :: String -> OutputPattern k
  Sequence :: OutputPattern k -> OutputPattern k -> OutputPattern k
  ResultOf :: forall (tk :: TermKind) a. (Typeable a, Show a) => Term tk a -> OutputPattern 'SpecificationP
  -- 'unevaluated' feedback group
  FeedbackGroupU :: forall (tk :: TermKind) a. (Typeable a, Show a) => Term tk a -> (a -> String) -> [OutputPattern 'SpecificationP] -> OutputPattern 'SpecificationP
  -- feedback group with constant feedback message
  FeedbackGroupE :: String -> [OutputPattern k] -> OutputPattern k

data WildcardType = NonEmpty | MaybeEmpty
  deriving (Eq,Ord,Show)

data PatternKind = SpecificationP | TraceP

-- k ~ 'SpecificationP is the biggest possible version of OutputPattern
convert :: OutputPattern k -> OutputPattern 'SpecificationP
convert (Wildcard b) = Wildcard b
convert (Text s) = Text s
convert (Sequence x y) = Sequence (convert x) (convert y)
convert (ResultOf t) = ResultOf t
convert (FeedbackGroupU t f grp) = FeedbackGroupU t f grp
convert (FeedbackGroupE str grp) = FeedbackGroupE str $ map convert grp

wildcard :: OutputPattern k
wildcard = Wildcard MaybeEmpty

nonEmptyWildcard :: OutputPattern k
nonEmptyWildcard = Wildcard NonEmpty

text :: String -> OutputPattern k
text = Text

resultOf :: (Typeable a, Show a) => Term tk a -> OutputPattern 'SpecificationP
resultOf = ResultOf

-- | Mandatory decoration of some term's result
--
-- Conceptually we have
--
-- > writeOutput [decoratedResultOf t]
-- > = writeOutput [nonEmptyWildcard <> resultOf t <> wildcard, wildcard <> resultOf t <> nonEmptyWildcard]
--
-- but matching failure will produce a specialized message
decoratedResultOf :: (Typeable a, Show a) => Term tk a -> OutputPattern 'SpecificationP
decoratedResultOf t = patternGroup t
  (\v -> "<output containing "++ show v ++" and some decoration>")
  (\p -> [nonEmptyWildcard <> p <> wildcard, wildcard <> p <> nonEmptyWildcard])

patternGroup :: (Typeable a, Show a) => Term tk a -> (a -> String) -> (OutputPattern 'SpecificationP -> [OutputPattern 'SpecificationP]) -> OutputPattern 'SpecificationP
patternGroup t f ps
  | all (null . valueTerms) (ps mempty) = FeedbackGroupU t f (ps $ resultOf t)
  | otherwise = error "patternGroup: patterns contain terms not specified in the first argument"

instance Eq (OutputPattern k) where
  x == y = compare x y == EQ

-- syntactic ordering (to put OutputPatterns in Sets)
-- Wildcard < Text < Sequence < ResultOf < FeedbackGroupU < FeedbackGroupE
instance Ord (OutputPattern k) where
  compare (Wildcard t) (Wildcard u) = compare t u
  compare (Text s) (Text t) = compare s t
  compare (Sequence x1 x2) (Sequence y1 y2) = compare (x1,x2) (y1,y2)
  compare (ResultOf (t :: Term k1 a)) (ResultOf (u :: Term k2 b)) =
    case gcompare (typeRep @a) (typeRep @b) of
      GLT -> LT
      GEQ -> compareK t u
      GGT -> GT
  compare (FeedbackGroupU _ _ grp1) (FeedbackGroupU _ _ grp2) = compare grp1 grp2 -- ASSUMPTION: groups are uniquely determined by their patterns
  compare (FeedbackGroupE x grp1) (FeedbackGroupE y grp2) = compare (x,grp1) (y,grp2)
  --
  compare (Wildcard _) _ = LT     -- Wildcard < {Text,Sequence,ResultOf,FeedbackGroup}
  compare _ (Wildcard _) = GT     -- {Text,Sequence,ResultOf,FeedbackGroup} > Wildcard
  compare Text{} _ = LT           -- Text < {Sequence,ResultOf,FeedbackGroup}
  compare _ Text{} = GT           -- {Sequence,ResultOf,FeedbackGroup} > Text
  compare Sequence{} _ = LT       -- Sequence < {ResultOf,FeedbackGroup}
  compare _ Sequence{} = GT       -- {ResultOf,FeedbackGroup} > Sequence
  compare ResultOf{} _ = LT       -- ResultOf < {FeedbackGroup}
  compare _ ResultOf{} = GT       -- {FeedbackGroup} > ResultOf
  compare FeedbackGroupU{} _ = LT -- FeedbackGroupU < {FeedbackGroupE}
  compare _ FeedbackGroupU{} = GT -- {FeedbackGroupE} > FeedbackGroupU

deriving instance Show (OutputPattern 'TraceP)

instance Semigroup WildcardType where
  NonEmpty <> _ = NonEmpty
  _ <> NonEmpty = NonEmpty
  MaybeEmpty <> MaybeEmpty = MaybeEmpty

instance Semigroup (OutputPattern k) where
  Wildcard t <> Wildcard u = Wildcard (t <> u)
  Wildcard t <> Sequence (Wildcard u) y = Sequence (Wildcard (t <> u)) y
  Text "" <> y = y
  x <> Text "" = x
  Text s <> Text t = Text $ s ++ t
  Sequence x y <> z = Sequence x $ y <> z
  x <> y = Sequence x y

instance Monoid (OutputPattern k) where
  mempty = Text ""

valueTerms :: OutputPattern k -> [SomeTermK]
valueTerms (Wildcard _) = []
valueTerms Text{} = []
valueTerms (Sequence x y) = valueTerms x ++ valueTerms y
valueTerms (ResultOf t) = [someTermK t]
valueTerms (FeedbackGroupU _ _ grp) = concatMap valueTerms grp
valueTerms (FeedbackGroupE _ grp) = concatMap valueTerms grp

evalPattern :: ValueMap -> OutputPattern k -> (OverflowWarning, OutputPattern 'TraceP)
evalPattern _ (Wildcard t) = (NoOverflow, Wildcard t)
evalPattern _ (Text s) = (NoOverflow, Text s)
evalPattern e (Sequence x y) = evalPattern e x <> evalPattern e y
evalPattern e (ResultOf t) = second (Text . showResult) $ oEval e t
evalPattern e (FeedbackGroupE str grp) = FeedbackGroupE str `second` traverse (evalPattern e) grp
evalPattern e (FeedbackGroupU t f grp) = FeedbackGroupE str `second` traverse (evalPattern e) grp
  where
    -- ASSUMPTION: t is already part of the pattern os overflows are ignored here
    str = f . snd $ oEval e t

type AddLinebreaks = Bool

evalPatternSet :: ValueMap -> Set (OutputPattern k) -> (OverflowWarning, Set (OutputPattern TraceP))
evalPatternSet = evalPatternSet' True

evalPatternSet' ::  AddLinebreaks -> ValueMap -> Set (OutputPattern k) -> (OverflowWarning, Set (OutputPattern 'TraceP))
evalPatternSet' addLinebreaks e = Set.foldr phi (mempty, Set.empty)
  where
    phi :: OutputPattern k -> (OverflowWarning, Set (OutputPattern 'TraceP)) -> (OverflowWarning, Set (OutputPattern 'TraceP))
    phi p (w,set) =
      let (w', p') = evalPattern e p
      in (w' <> w, Set.fromList (p' : [p'<>text "\n" | addLinebreaks]) `Set.union` set)

showPattern :: OutputPattern k -> String
showPattern (Wildcard NonEmpty) = "<some text>"
showPattern (Wildcard MaybeEmpty) = "<anything>"
showPattern (Text s) = foldr showLitChar "" s
showPattern (Sequence x y) = showPattern x ++ showPattern y
showPattern (ResultOf t) = show t
showPattern (FeedbackGroupU _ _ grp) = "<unevaluated group: " <> show (map showPattern grp) <> ">"
showPattern (FeedbackGroupE str _) = str

showPatternSimple :: OutputPattern k -> String
showPatternSimple p =
  case reverse $ showPattern p of
  ('n':'\\':s) -> reverse s
  s -> reverse s

-- | coverage relation on patterns
(>:) :: OutputPattern 'TraceP -> OutputPattern 'TraceP -> Bool
p >: q = isRight $ parse (patternParser p <> eof) "" $ showPatternInternal q

-- hack to allow coverage check through parsing
-- (during coverage testing this functions is only called with Text arguments produced by the tested program)
showPatternInternal :: OutputPattern 'TraceP -> String
showPatternInternal (Wildcard NonEmpty) = "_"
showPatternInternal (Wildcard MaybeEmpty) = " "
showPatternInternal (Text s) = s
showPatternInternal (Sequence x y) = showPatternInternal x ++ showPatternInternal y
showPatternInternal _ = error "showPatternInternal: not intended"

patternParser :: OutputPattern 'TraceP -> Parsec String () ()
patternParser (Wildcard NonEmpty) = void $ many (whitespace <|> nonPrintable) >> printableNonWhitespace >> patternParser wildcard
patternParser (Wildcard MaybeEmpty) = void $ many anyChar
patternParser (Text s) = void $ string s
patternParser (Sequence (Wildcard NonEmpty) q) = many (whitespace <|> nonPrintable) >> printableNonWhitespace >> patternParser (wildcard <> q)
patternParser p@(Sequence (Wildcard MaybeEmpty) q) = try (anyChar >> patternParser p) <|> patternParser q
patternParser (Sequence p q) = patternParser p >> patternParser q
patternParser (FeedbackGroupE _ grp) = choice $ map (try . patternParser) grp

printableNonWhitespace :: Parsec String () Char
printableNonWhitespace = satisfy $ \c -> not (isSpace c) && isPrint c

whitespace :: Parsec String () Char
whitespace = satisfy $ \c -> isSpace c && isPrint c

nonPrintable :: Parsec String () Char
nonPrintable = satisfy (not . isPrint)
