{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTasks.OutputPattern where

import Prelude hiding (all)

import Test.IOTasks.Overflow
import Test.IOTasks.OutputTerm
import Test.IOTasks.ValueMap

import Data.Either (isRight)
import Data.Bifunctor (second)
import Data.Typeable

import Text.Parsec
import Data.Char (isPrint, showLitChar)
import Control.Monad (void)

data OutputPattern (t :: PatternType) where
  Wildcard :: OutputPattern t
  Text :: String -> OutputPattern t
  Sequence :: OutputPattern t -> OutputPattern t -> OutputPattern t
  Value :: (OverflowType a,Show a) => OutputTerm a -> OutputPattern 'SpecificationP

data PatternType = SpecificationP | TraceP

instance Eq (OutputPattern t) where
  x == y = compare x y == EQ

-- syntactic ordering (to put OutputPatterns in Sets)
instance Ord (OutputPattern t) where
  compare Wildcard Wildcard = EQ
  compare Wildcard _ = LT
  compare _ Wildcard = GT
  compare (Text s) (Text t) = compare s t
  compare Text{} _ = LT
  compare Sequence{} Text{} = GT
  compare (Sequence x1 x2) (Sequence y1 y2) =
    case compare x1 y1 of
      EQ -> compare x2 y2
      r -> r
  compare (Value (t :: OutputTerm a)) (Value (u :: OutputTerm b)) =
    case eqT @a @b of
      Just Refl -> compare t u
      Nothing -> compare (typeRep (Proxy @a)) (typeRep (Proxy @b))
  compare Value{} _ = GT
  compare _ Value{} = LT

deriving instance Show (OutputPattern t)

instance Semigroup (OutputPattern t) where
  Wildcard <> Wildcard = Wildcard
  Wildcard <> Sequence Wildcard y = Sequence Wildcard y
  Text "" <> y = y
  x <> Text "" = x
  Text s <> Text t = Text $ s ++ t
  Sequence x y <> z = Sequence x $ y <> z
  x <> y = Sequence x y

instance Monoid (OutputPattern t) where
  mempty = Text ""

valueTerms :: OutputPattern t -> [SomeOutputTerm]
valueTerms Wildcard = []
valueTerms Text{} = []
valueTerms (Sequence x y) = valueTerms x ++ valueTerms y
valueTerms (Value t) = [SomeOutputTerm t]

evalPattern :: ValueMap -> OutputPattern t -> (OverflowWarning, OutputPattern 'TraceP)
evalPattern _ Wildcard = (NoOverflow, Wildcard)
evalPattern _ (Text s) = (NoOverflow, Text s)
evalPattern e (Sequence x y) = evalPattern e x <> evalPattern e y
evalPattern e (Value t) = second (Text . showValue) $ eval t e

printPattern :: OutputPattern t -> String
printPattern Wildcard = "_"
printPattern (Text s) = foldr showLitChar "" s
printPattern (Sequence x y) = printPattern x ++ printPattern y
printPattern (Value t) = show t

printPatternSimple :: OutputPattern t -> String
printPatternSimple p =
  case reverse $ printPattern p of
  ('n':'\\':s) -> reverse s
  s -> reverse s

(>:) :: OutputPattern 'TraceP -> OutputPattern 'TraceP -> Bool
p >: q = isRight $ parse (patternParser p <> eof) "" $ printPattern q

patternParser :: OutputPattern 'TraceP -> Parsec String () ()
patternParser Wildcard = void $ many (satisfy isPrint)
patternParser (Text s) = void $ string (foldr showLitChar "" s)
patternParser p@(Sequence Wildcard q) = try (anyChar >> patternParser p) <|> patternParser q
patternParser (Sequence p q) = patternParser p >> patternParser q
