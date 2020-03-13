{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.Pattern
  ( Pattern (..)
  , var
  , whitespace
  , buildPattern
  , buildTermPattern
  , FixedPattern
  , TermPattern
  , fillHoles
  , emptyPattern
  , isContainedIn
  ) where

import Test.IOTasks.Environment
import Test.IOTasks.Term (SemTerm(..))
import Test.IOTasks.Utils

import Data.Coerce
import Data.Functor (void)
import Data.List (intersperse)
import Data.Maybe (isJust, fromJust)

import Test.QuickCheck
import Text.Parsec
import Text.Parsec.Char (endOfLine)
import Text.Parsec.String
import Text.PrettyPrint.HughesPJClass (Pretty)
import qualified Text.PrettyPrint.HughesPJClass as PP

-- ----------------------------- --
-- TODO: reduce code duplication --
-- ----------------------------- --

newtype TermPattern = TermPattern [SimplePattern 'WithVars]
  deriving (Eq,Ord)
  deriving (Pretty, Semigroup, Monoid, Pattern) via (SimplePatternList 'WithVars)
newtype FixedPattern = FixedPattern [SimplePattern 'NoVars]
  deriving (Eq,Ord)
  deriving (Pretty, Semigroup, Monoid, Pattern) via (SimplePatternList 'NoVars)
-- Ord instances are mainly for putting pattern into Sets

data PType = WithVars | NoVars

data SimplePattern (t :: PType) where
  WildCard :: SimplePattern t
  Literal :: String -> SimplePattern t
  Hole :: Int -> SimplePattern 'WithVars
  Linebreak :: SimplePattern t
  NoBreak :: SimplePattern t
  Backslash :: SimplePattern t

deriving instance Eq (SimplePattern t)
deriving instance Ord (SimplePattern t)

emptyPattern :: FixedPattern
emptyPattern = mempty

class Monoid p => Pattern p where
  -- forced linebreak
  infixl 6 $$
  ($$) :: p -> p -> p
  -- wildcard
  anything :: p
  -- literal, matches the given string even if its words are seperated by linebreaks
  text :: String -> p
  -- non breakable text (can not be interrupted by a linebreak)
  lineText :: String -> p
  -- composition with added whitespace
  infixl 5 <+>
  (<+>) :: p -> p -> p
  -- non breaking composition with added whitespace
  infixl 5 <~>
  (<~>) :: p -> p -> p
  -- linebreak
  linebreak :: p

whitespace :: Pattern p => p
whitespace = text " "

var :: Int -> TermPattern
var n | n < 0     = error "var: negative variable index"
      | otherwise = TermPattern [Hole n]

instance Pattern (SimplePatternList t) where
  (SPList ps) $$ (SPList qs) = SPList (ps ++ [Linebreak] ++ qs)
  anything = SPList [WildCard]
  text = escapeLiteral
  lineText s
    | '\n' `elem` s = error "lineText: non breakable text can not contain linebreaks"
    | otherwise  = let SPList xs = escapeLiteral s in SPList (intersperse NoBreak xs)
  p <+> q = p <> SPList [Literal " "] <> q
  p <~> q = p <> SPList [NoBreak] <> q
  linebreak = SPList [Linebreak]

escapeLiteral :: String -> SimplePatternList t
escapeLiteral = foldMap $ \case
  '\n' -> SPList [Linebreak]
  '\\' -> SPList [Backslash]
  c -> SPList [Literal [c]]

instance Show FixedPattern where
  show (FixedPattern []) = "emptyPattern"
  show p = "buildPattern " <> PP.render (PP.doubleQuotes $ PP.pPrint p)

instance Show TermPattern where
  show p = "buildTermPattern " <> PP.render (PP.doubleQuotes $ PP.pPrint p)

instance Show (SimplePatternList t) where
  show p = "buildPattern " <> PP.render (PP.doubleQuotes $ PP.pPrint p)

instance Pretty (SimplePatternList t) where
  pPrint (SPList []) = PP.text "emptyTermPattern"
  pPrint (SPList xs) = foldr (\x ys -> PP.pPrint x <> ys) mempty xs

instance Pretty (SimplePattern l) where
  pPrint WildCard = PP.text "_"
  pPrint (Literal l) = PP.text (newlineToWhitespace l)
  pPrint (Hole n) = PP.text "#" <> PP.pPrint n
  pPrint Linebreak = PP.text "\\n"
  pPrint NoBreak = PP.text "~"
  pPrint Backslash = PP.text "\\\\"

newlineToWhitespace :: String -> String
newlineToWhitespace = map (\c -> if c == '\n' then ' ' else c)

newtype SimplePatternList t = SPList { _fromSPList :: [SimplePattern t] }

instance Semigroup (SimplePatternList t) where
  SPList [] <> p = p
  p <> SPList [] = p
  SPList xs <> SPList (y:ys) =
    let xs' = init xs
        x = last xs
    in SPList $ xs' ++ op x y ++ ys

op :: SimplePattern t -> SimplePattern t -> [SimplePattern t]
op WildCard WildCard = [WildCard]
op (Literal l1) (Literal l2) = [Literal (l1 ++ l2)]
op p1 p2 = [p1,p2]

instance Monoid (SimplePatternList t) where
  mempty = SPList []

fillHoles :: (SemTerm t, StringEmbedding a) => (TermPattern, [t a]) -> Environment -> FixedPattern
fillHoles (TermPattern xs, ts) d = mconcat $ (\s -> coerce $ fillSimple (s, ts) d) <$> xs

fillSimple :: (SemTerm t, StringEmbedding a) => (SimplePattern 'WithVars, [t a]) -> Environment -> SimplePatternList 'NoVars
fillSimple (WildCard, _) _ = SPList [WildCard]
fillSimple (Literal p, _) _ = SPList [Literal p]
fillSimple (Hole n, ts) d = text . pack $ evalTerm (ts !! n) d
fillSimple (Linebreak, _) _ = SPList [Linebreak]
fillSimple (NoBreak, _) _ = SPList [NoBreak]
fillSimple (Backslash, _) _ = SPList [Backslash]

isContainedIn :: String -> FixedPattern -> Bool
isContainedIn s p = parse (patternParser p) "" s == Right ()

-- yields a parser that succesfully parses a string matched by the given pattern
patternParser :: FixedPattern -> Parser ()
patternParser pat = patternParser' pat >> eof where
  patternParser' (FixedPattern [])                = void $ many endOfLine
  -- patternParser' (FixedPattern [Literal l])       = mapM_ (\c -> if c == ' ' then char ' ' <|> ('\n' <$ many1 endOfLine) else char c) l >> (many endOfLine >> eof)
  patternParser' (FixedPattern (Literal l : p2))  = mapM_ (\c -> if c == ' ' then char ' ' <|> ('\n' <$ many1 endOfLine) else char c <|> (('\n' <$ many1 endOfLine) >> char c)) l >> patternParser' (FixedPattern p2)
  patternParser' p@(FixedPattern (WildCard : p2)) = try (anyChar >> patternParser' p) <|> patternParser' (FixedPattern p2)
  patternParser' (FixedPattern (Linebreak : p2))  = endOfLine >> patternParser' (FixedPattern p2)
  patternParser' (FixedPattern (NoBreak : p2))    = char ' ' >> notFollowedBy endOfLine >> patternParser' (FixedPattern p2)
  patternParser' (FixedPattern (Backslash : p2))  = string "\\" >> patternParser' (FixedPattern p2)

-- tests
_simpleProp :: IO ()
_simpleProp = quickCheckWith stdArgs{maxSuccess = 1000} $ \xs -> isContainedIn xs (text xs)

_test :: IO ()
_test = quickCheck $ forAll (listOf $ elements ['A'..'~']) $ \xs ->
  let str = mergeWildCards $ concatMap @[] (\c -> if c == '#' then "#1" else [c]) xs
  in isJust (buildPattern str) ==> show (fromJust (buildPattern str)) == "buildPattern " ++ "\"" ++ str ++ "\""

mergeWildCards :: String -> String
mergeWildCards = foldr f "" where
  f '_' ('_':xs) = '_':xs
  f x   xs       = x:xs

-- pattern construction from Strings
buildPattern :: String -> Maybe FixedPattern
buildPattern s = either (const Nothing) Just $ parse parsePattern "" s

parsePattern :: Parser FixedPattern
parsePattern =
  (do
    p <- parsePattern'
    (%%) <- ((<~>) <$ char '~') <|> ((<+>) <$ char ' ') <|> ($$) <$ char '\n' <|> pure (<>)
    q <- parsePattern
    return $ p %% q)
  <|> text "" <$ eof

parsePattern' :: Parser FixedPattern
parsePattern' =
      anything <$ char '_'
  <|> text <$> many1 (noneOf ['~',' ','_'])

buildTermPattern :: String -> Maybe TermPattern
buildTermPattern s = either (const Nothing) Just $ parse parseTermPattern "" s

parseTermPattern :: Parser TermPattern
parseTermPattern =
  (do
    p <- parseTermPattern'
    (%%) <- ((<~>) <$ char '~') <|> ((<+>) <$ char ' ') <|> ($$) <$ char '\n' <|> pure (<>)
    q <- parseTermPattern
    return $ p %% q)
  <|> text "" <$ eof

parseTermPattern' :: Parser TermPattern
parseTermPattern' =
      anything <$ char '_'
  <|> try (text <$> many1 (noneOf ['~',' ','_', '#']))
  <|> var . read <$> (char '#' >> (string "0" <|> many1 digit))
