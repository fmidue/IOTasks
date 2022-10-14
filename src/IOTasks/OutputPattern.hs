{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.OutputPattern where

import Prelude hiding (all)

import IOTasks.Terms (Varname)
import IOTasks.Overflow
import IOTasks.OutputTerm

import Data.Either (isRight)
import Data.Map (Map)

import Text.Parsec
import Data.Char (isPrint, showLitChar)
import Control.Monad (void)

data OutputPattern (t :: PatternType) where
  Wildcard :: OutputPattern t
  Text :: String -> OutputPattern t
  Sequence :: OutputPattern t -> OutputPattern t -> OutputPattern t
  Value :: OutputTerm Integer -> OutputPattern 'SpecificationP

data PatternType = SpecificationP | TraceP

deriving instance Eq (OutputPattern t)
deriving instance Ord (OutputPattern t)
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

evalPattern :: Map Varname [(Integer,Int)] -> OutputPattern t -> (OverflowWarning, OutputPattern 'TraceP)
evalPattern _ Wildcard = (NoOverflow, Wildcard)
evalPattern _ (Text s) = (NoOverflow, Text s)
evalPattern e (Sequence x y) = evalPattern e x <> evalPattern e y
evalPattern e (Value t) = let x = eval t e in (checkOverflow (fromInteger x),Text $ show x)

printPattern :: OutputPattern 'TraceP -> String
printPattern Wildcard = "_"
printPattern (Text s) = foldr showLitChar "" s
printPattern (Sequence x y) = printPattern x ++ printPattern y

printPatternSimple :: OutputPattern 'TraceP -> String
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
