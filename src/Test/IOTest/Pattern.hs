{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Pattern
  ( buildPattern
  , buildTermPattern
  , Pattern
  , TermPattern
  , fillHoles
  , emptyPattern
  , isSubPatternOf
  , isContainedIn
  ) where

import Test.IOTest.Environment
import Test.IOTest.Term (Term, evalTerm)
import Test.IOTest.Utils

import Data.String
import Data.Functor

import Test.QuickCheck
import Text.Parsec
import Text.Parsec.String
import Text.PrettyPrint.HughesPJClass hiding ((<>))

-- ----------------------------- --
-- TODO: reduce code duplication --
-- ----------------------------- --

newtype TermPattern = TermPattern [SimplePattern 'WithVars] deriving (Eq,Ord)
newtype Pattern = Pattern [SimplePattern 'NoVars] deriving (Eq,Ord)
-- Ord instances are mainly for putting pattern into Sets

data PType = WithVars | NoVars

data SimplePattern (t :: PType) where
  WildCard :: SimplePattern t
  Literal :: String -> SimplePattern t
  Hole :: Int -> SimplePattern 'WithVars

deriving instance Eq (SimplePattern t)
deriving instance Ord (SimplePattern t)

emptyPattern :: Pattern
emptyPattern = mempty

instance Pretty Pattern where
  pPrint (Pattern []) = text "emptyPattern"
  pPrint (Pattern xs) = foldr (\x ys -> pPrint x <> ys) mempty xs

instance Show Pattern where
  show (Pattern []) = "emptyPattern"
  show p = "buildPattern " <> render (doubleQuotes $ pPrint p)

instance Pretty TermPattern where
  pPrint (TermPattern []) = text "emptyTermPattern"
  pPrint (TermPattern xs) = foldr (\x ys -> pPrint x <> ys) mempty xs

instance Show TermPattern where
  show p = "buildTermPattern " <> render (doubleQuotes $ pPrint p)

instance Pretty (SimplePattern l) where
  pPrint WildCard = text "_"
  pPrint (Literal l) = text (escapeNewline l)
  pPrint (Hole n) = text "#" <> pPrint n

escapeNewline :: String -> String
escapeNewline = concatMap (\c -> if c == '\n' then "\\n" else [c])

-- buildPattern always results in an non-emtpy Pattern
buildPattern :: String -> Pattern
buildPattern "_" = Pattern [WildCard]
buildPattern ('_':xs) = Pattern [WildCard] <> buildPattern xs
buildPattern xs =
  let (lit,rest) = span (/= '_') xs
  in if null rest
    then Pattern [Literal lit]
    else Pattern [Literal lit] <> buildPattern rest

-- buildTermPattern always results in an non-emtpy Pattern
buildTermPattern :: String -> TermPattern
buildTermPattern "_" = TermPattern [WildCard]
buildTermPattern ('_':xs) = TermPattern [WildCard] <> buildTermPattern xs
buildTermPattern ('#':n:xs) = TermPattern [Hole $ read [n]] <> buildTermPattern xs
buildTermPattern xs =
  let (lit,rest) = span (/= '_') xs
  in if null rest
    then TermPattern [Literal lit]
    else TermPattern [Literal lit] <> buildTermPattern rest

instance IsString Pattern where
  fromString = buildPattern

instance IsString TermPattern where
  fromString = buildTermPattern

instance Semigroup Pattern where
  Pattern [] <> p = p
  p <> Pattern [] = p
  Pattern xs <> Pattern (y:ys) =
    let xs' = init xs
        x = last xs
    in Pattern $ xs' ++ op x y ++ ys

instance Monoid Pattern where
  mempty = Pattern []

instance Semigroup TermPattern where
  TermPattern [] <> p = p
  p <> TermPattern [] = p
  TermPattern xs <> TermPattern (y:ys) =
    let xs' = init xs
        x = last xs
    in TermPattern $ xs' ++ op x y ++ ys

instance Monoid TermPattern where
  mempty = TermPattern []

op :: SimplePattern t -> SimplePattern t -> [SimplePattern t]
op WildCard WildCard = [WildCard]
op (Literal l1) (Literal l2) = [Literal (l1 ++ l2)]
op p1 p2 = [p1,p2]

fillHoles :: StringEmbedding a => (TermPattern, [Term a]) -> Environment -> Pattern
fillHoles (TermPattern xs, ts) d = Pattern $ (\s -> fillSimple (s, ts) d) <$> xs

fillSimple :: StringEmbedding a => (SimplePattern 'WithVars, [Term a]) -> Environment -> SimplePattern 'NoVars
fillSimple (WildCard, _) _ = WildCard
fillSimple (Literal p, _) _ = Literal p
fillSimple (Hole n, ts) d = Literal . pack $ evalTerm (ts !! n) d

isSubPatternOf :: Pattern -> Pattern ->  Bool
p1 `isSubPatternOf` p2 = parse (patternParser p2) "" (render $ pPrint p1) == Right ()

isContainedIn :: String -> Pattern -> Bool
isContainedIn s p = buildPattern s `isSubPatternOf` p

-- yields a parser that succesfully parses the string representation of a
-- pattern less general than the given pattern
patternParser :: Pattern -> Parser ()
patternParser pat = patternParser' pat >> eof where
  patternParser' (Pattern []) = void $ string ""
  patternParser' (Pattern [Literal l]) = void $ string (escapeNewline l)
  patternParser' (Pattern [WildCard]) = void $ many anyChar
  patternParser' (Pattern (Literal l : p2)) = string (escapeNewline l) >> patternParser' (Pattern p2)
  patternParser' p@(Pattern (WildCard : p2)) = try (anyChar >> patternParser' p) <|> patternParser' (Pattern p2)

-- tests
_test :: IO ()
_test = quickCheck $ \xs -> let str = concatMap @[] (\c -> if c == '#' then "#1" else [c]) xs in render (pPrint (fromString @Pattern str)) == str
