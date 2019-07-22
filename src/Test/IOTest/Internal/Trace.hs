{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.Internal.Trace (
  NTrace,
  Trace,
  Trace'(..),
  normalize,
  isCoveredBy,
  showNTrace,
  inputs,
) where

import Test.IOTest.Internal.Pattern

import Control.Monad.Trans.Writer

import Data.Set (Set)
import qualified Data.Set as S
import           Data.List

data Trace' o
  = ProgRead String (Trace' o)
  | ProgWrite o (Trace' o)
  | Stop
  | OutOfInputs
  deriving Functor

type Trace = Trace' String
type NTrace = Trace' (Set LinearPattern)


ppMismatch :: (Show a1, Show a2) => Set a1 -> Set a2 -> String
ppMismatch xs ys = "Mismatch while comparing patterns: " ++ show (S.toList xs) ++ " is not covered by " ++ show (S.toList ys)

normalize :: Trace -> NTrace
normalize = go emptyPattern where
  go p (ProgWrite v t') = go (p <> buildPattern v) t'
  go p (ProgRead v t') = ProgWrite (S.singleton p) $ ProgRead v $ go emptyPattern t'
  go p Stop = ProgWrite (S.singleton p) Stop
  go p OutOfInputs = ProgWrite (S.singleton p) OutOfInputs

isCoveredBy :: NTrace -> NTrace -> Writer String Bool
ProgRead x t1 `isCoveredBy` ProgRead y t2 =
  if x == y
    then t1 `isCoveredBy` t2
    else do
      tell $ "Input mismatch: Expected " ++ show x ++ ". But got " ++ show y
      return False
ProgWrite v1 t1' `isCoveredBy` ProgWrite v2 t2' =
  if v1 `isSubsetOf` v2
    then t1' `isCoveredBy` t2'
    else do
      tell $ ppMismatch v1 v2
      return False
Stop `isCoveredBy` Stop = return True
_ `isCoveredBy` _ = tell "traces dont line up" >> return False

isSubsetOf :: Set LinearPattern -> Set LinearPattern -> Bool
x `isSubsetOf` y = x == y || all (\px -> any (\py -> px `isSubPatternOf` py) (S.toList y)) (S.toList x)
                          -- ^           ^        ^- such that
                          -- '           '- there exist an element py in y
                          -- '- forall elements px in x

instance Show Trace where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

instance Show NTrace where
  show = showNTrace

showNTrace :: NTrace -> String
showNTrace (ProgRead v t) = "?"++show v++" "++showNTrace t
showNTrace (ProgWrite v t) =
  let vs = (\x -> if x == "[]" then "e" else x) . show <$> S.toList v
  in "!{"++ intercalate "," vs ++"} "++showNTrace t
showNTrace Stop = "stop"
showNTrace OutOfInputs = "<out of inputs>"

similar :: Trace' a -> Trace' b -> Bool
similar x y = inputs x == inputs y

ppOutputMismatch :: Show a => [a] -> [a] -> String
ppOutputMismatch [v1] v2 =
  let clean :: Show a => a -> String
      clean = filter (`notElem` ['\\','\"']) . show
  in case v2 of
      [v2'] -> "Output mismatch: Expected " ++ clean v2' ++ ". But got " ++ clean v1
      _ ->  "Output mismatch: Expected one of " ++ clean v2 ++ ". But got " ++ clean v1
ppOutputMismatch _ _ = error "should not happen"

inputs :: Trace' a -> [String]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
