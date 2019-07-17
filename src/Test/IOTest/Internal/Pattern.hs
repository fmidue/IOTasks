{-# LANGUAGE TypeApplications #-}
module Test.IOTest.Internal.Pattern
  ( matches
  , buildPattern
  , LinearPattern
  ) where

import           Test.QuickCheck

import Data.String

import Text.Regex.Posix

data LinearPattern
  = Simple SimplePattern
  | Sequence SimplePattern LinearPattern

data SimplePattern = Empty | WildCard | Literal String

instance Show LinearPattern where
  show (Simple s) = show s
  show (Sequence p1 p2) = show p1 ++ show p2

instance Show SimplePattern where
  show Empty = ""
  show WildCard = "_"
  show (Literal l) = l

matches :: String -> LinearPattern -> Bool
matches xs p = xs =~ ("^" ++ regexString p ++ "$")

regexString :: LinearPattern -> String
regexString (Simple s) = simpleRegexString s
regexString (Sequence p1 p2) = simpleRegexString p1 ++ regexString p2

simpleRegexString :: SimplePattern -> String
simpleRegexString Empty = ""
simpleRegexString WildCard = ".*"
simpleRegexString (Literal p) = p

buildPattern :: String -> LinearPattern
buildPattern "" = Simple Empty
buildPattern ['_'] = Simple WildCard
buildPattern ('_':xs) = Sequence WildCard (buildPattern xs)
buildPattern xs =
  let (lit,rest) = span (/= '_') xs
  in if null rest
    then Simple (Literal lit)
    else Sequence (Literal lit) (buildPattern rest)

instance IsString LinearPattern where
  fromString = buildPattern

instance Semigroup LinearPattern where
  Simple Empty        <> p                   = p
  p                   <> Simple Empty        = p
  Simple WildCard     <> Simple WildCard     = Simple WildCard
  Simple (Literal l1) <> Simple (Literal l2) = Simple $ Literal (l1 <> l2)
  Simple p1           <> Simple p2           = Sequence p1 (Simple p2)
  Simple p1           <> Sequence p21 p22    = Sequence p1 (Sequence p21 p22)
  Sequence p11 p12    <> p2                  = Sequence p11 (p12 <> p2)

instance Monoid LinearPattern where
  mempty = Simple Empty

-- tests
_test :: IO ()
_test = quickCheck $ \xs -> show (fromString @LinearPattern xs) == xs
