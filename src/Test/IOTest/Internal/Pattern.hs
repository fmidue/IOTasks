{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Internal.Pattern
  ( buildPattern
  , LinearPattern
  , hasHoles
  , fillHoles
  , emptyPattern
  , isSubPatternOf
  ) where

import Test.IOTest.Internal.Environment
import Test.IOTest.Internal.Term
import Test.IOTest.Utils

import Data.String
import Data.Functor
import Data.Proxy

import Test.QuickCheck
import Text.Regex.Posix
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.PrettyPrint.HughesPJClass hiding ((<>))

data LinearPattern
  = Simple SimplePattern
  | Sequence SimplePattern LinearPattern
  deriving (Eq,Ord)

emptyPattern :: LinearPattern
emptyPattern = Simple Empty

data SimplePattern
  = Empty
  | WildCard
  | Literal String
  | Hole Int
  deriving (Eq,Ord)

instance Pretty LinearPattern where
  pPrint (Simple s) = pPrint s
  pPrint (Sequence p1 p2) = pPrint p1 <> pPrint p2

instance Pretty SimplePattern where
  pPrint Empty = text ""
  pPrint WildCard = text "_"
  pPrint (Literal l) = text l
  pPrint (Hole n) = text "#" <> pPrint n

instance Show LinearPattern where
  show p = "buildPattern " <> render (doubleQuotes $ pPrint p)

hasHoles :: LinearPattern -> Bool
hasHoles (Simple (Hole _)) = True
hasHoles (Simple _) = False
hasHoles (Sequence (Hole _) _) = True
hasHoles (Sequence _ p2) = hasHoles p2

-- returns Nothing if the pattern contains at least one hole
matches :: String -> LinearPattern -> Maybe Bool
matches xs p = regexString p <&> (\s -> xs =~ ("^" ++ s ++ "$"))

regexString :: LinearPattern -> Maybe String
regexString (Simple s) = simpleRegexString s
regexString (Sequence p1 p2) = (++) <$> simpleRegexString p1 <*> regexString p2

simpleRegexString :: SimplePattern -> Maybe String
simpleRegexString Empty = Just ""
simpleRegexString WildCard = Just ".*"
simpleRegexString (Literal p) = Just p
simpleRegexString (Hole _) = Nothing

buildPattern :: String -> LinearPattern
buildPattern "" = Simple Empty
buildPattern "_" = Simple WildCard
buildPattern ('_':xs) = Simple WildCard <> buildPattern xs
buildPattern ('#':n:xs) = Simple (Hole $ read [n]) <> buildPattern xs
buildPattern xs =
  let (lit,rest) = span (/= '_') xs
  in if null rest
    then Simple (Literal lit)
    else Simple (Literal lit) <> buildPattern rest

buildPattern' :: Show a => a -> LinearPattern
buildPattern' = buildPattern . show

instance IsString LinearPattern where
  fromString = buildPattern

instance Semigroup LinearPattern where
  Simple Empty        <> p                         = p
  p                   <> Simple   Empty            = p
  Simple WildCard     <> Simple   WildCard         = Simple WildCard
  Simple WildCard     <> Sequence WildCard     p2  = Sequence WildCard p2
  Simple (Literal l1) <> Simple   (Literal l2)     = Simple $ Literal (l1 <> l2)
  Simple (Literal l1) <> Sequence (Literal l2) p2  = Simple (Literal (l1 <> l2)) <> p2
  Simple p1           <> Simple   p2               = Sequence p1 (Simple p2)
  Simple p1           <> Sequence p21          p22 = Sequence p1 (Sequence p21 p22)
  Sequence p11 p12    <>                       p2  = Sequence p11 (p12 <> p2)

instance Monoid LinearPattern where
  mempty = Simple Empty

matchesWithEnvironment :: StringEmbedding a => String -> (LinearPattern, [Term a]) -> Environment -> Maybe Bool
matchesWithEnvironment xs (p,ts) d = xs `matches` fillHoles (p,ts) d

fillHoles :: StringEmbedding a => (LinearPattern, [Term a]) -> Environment -> LinearPattern
fillHoles (Simple p, ts) d = Simple $ fillSimple (p,ts) d
fillHoles (Sequence p1 p2, ts) d = Simple (fillSimple (p1,ts) d) <> fillHoles (p2,ts) d

fillSimple :: StringEmbedding a => (SimplePattern, [Term a]) -> Environment -> SimplePattern
fillSimple (Empty, _) _ = Empty
fillSimple (WildCard, _) _ = WildCard
fillSimple (Literal p, _) _ = Literal p
fillSimple (Hole n, ts) d = Literal . pack $ evalTerm (ts !! n) d

isSubPatternOf :: LinearPattern -> LinearPattern ->  Bool
p1 `isSubPatternOf` p2 = parse (patternParser p2) "" (render $ pPrint p1) == Right ()

-- yields a parser that succesfully parses the string representation of a
-- pattern less general than the given pattern
patternParser :: LinearPattern -> Parser ()
patternParser pat = patternParser' pat >> eof where
  patternParser' (Simple Empty) = void $ string ""
  patternParser' (Simple (Literal l)) = void $ string l
  patternParser' (Simple (Hole n)) = void . string $ "#" <> show n
  patternParser' (Simple WildCard) = void $ many anyChar
  patternParser' (Sequence Empty p2) = string "" >> patternParser' p2
  patternParser' (Sequence (Literal l) p2) = string l >> patternParser' p2
  patternParser' (Sequence (Hole n) p2) = string ("#" <> show n) >> patternParser' p2
  patternParser' p@(Sequence WildCard p2) = try (anyChar >> patternParser' p) <|> patternParser' p2

-- tests
_test :: IO ()
_test = quickCheck $ \xs -> let str = concatMap @[] (\c -> if c == '#' then "#1" else [c]) xs in render (pPrint (fromString @LinearPattern str)) == str
