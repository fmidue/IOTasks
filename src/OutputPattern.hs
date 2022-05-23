{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module OutputPattern where

import Prelude hiding (all)

import Term (Varname)
import OutputTerm

import Data.Either (isRight)
import Data.Map (Map)

import Text.Parsec
import Data.Char (isPrint, showLitChar)

data OutputPattern (t :: PatternType) where
  Wildcard :: OutputPattern t
  Text :: String -> OutputPattern t
  Sequence :: OutputPattern t -> OutputPattern t -> OutputPattern t
  Value :: OutputTerm -> OutputPattern 'SpecificationP

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

evalPattern :: Map Varname [Integer] -> OutputPattern t -> OutputPattern 'TraceP
evalPattern _ Wildcard = Wildcard
evalPattern _ (Text s) = Text s
evalPattern e (Sequence x y) = evalPattern e x <> evalPattern e y
evalPattern e (Value t) = Text . show @Integer $ eval t e

printPattern :: OutputPattern 'TraceP -> String
printPattern Wildcard = "_"
printPattern (Text s) = foldr showLitChar "" s
printPattern (Sequence x y) = printPattern x ++ printPattern y

(>:) :: OutputPattern 'TraceP -> OutputPattern 'TraceP -> Bool
p >: q = isRight $ parse (patternParser p) "" $ printPattern q

patternParser :: OutputPattern 'TraceP -> Parsec String () ()
patternParser Wildcard = () <$ many (satisfy isPrint)
patternParser (Text s) = () <$ string (foldr showLitChar "" s)
patternParser p@(Sequence Wildcard q) = try (anyChar >> patternParser p) <|> patternParser q
patternParser (Sequence p q) = patternParser p >> patternParser q
