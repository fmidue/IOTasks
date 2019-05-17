{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.Internal.Trace (
  IntTrace,
  NTrace,
  Trace,
  Trace'(..),
  normalize,
  isCoveredBy,
  showNTrace,
  inputs,
  Normalizeable(..)
) where

import Test.IOTest.Internal.Pattern

import Control.Monad.Trans.Writer

import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as S
import           Data.List

data Trace' o i
  = ProgRead i (Trace' o i)
  | ProgWrite o (Trace' o i)
  | Stop
  | OutOfInputs
  deriving (Functor)

instance Bifunctor Trace' where
  bimap f g (ProgRead i t) = ProgRead (g i) (bimap f g t)
  bimap f g (ProgWrite o t) = ProgWrite (f o) (bimap f g t)
  bimap _ _ Stop = Stop
  bimap _ _ OutOfInputs = OutOfInputs

type IntTrace = Trace Int
type Trace a = Trace' a a
type NTrace a = Trace' (Set [ConcretePattern a]) a

class Normalizeable a where
  toPattern :: a -> ConcretePattern a
  leq :: Set [ConcretePattern a] -> Set [ConcretePattern a] -> Bool
  ppMismatch :: Set [ConcretePattern a] -> Set [ConcretePattern a] -> String

normalize :: Normalizeable a => Trace a -> NTrace a
normalize = go [] where
  go w (ProgWrite v t') = go (w ++ [toPattern v]) t'
  go w (ProgRead v t') = ProgWrite (S.singleton w) $ ProgRead v $ go [] t'
  go w Stop = ProgWrite (S.singleton w) Stop
  go w OutOfInputs = ProgWrite (S.singleton w) OutOfInputs

isCoveredBy :: (Show a, Eq a, Normalizeable a) => NTrace a -> NTrace a -> Writer String Bool
--isCoveredBy x y | trace ("coveredBy?\n" ++ show x ++ "\n\n" ++ show y) False = undefined
ProgRead x t1 `isCoveredBy` ProgRead y t2 =
  if x == y
    then t1 `isCoveredBy` t2
    else do
      tell $ "Input mismatch: Expected " ++ show x ++ ". But got " ++ show y
      return False
ProgWrite v1 t1' `isCoveredBy` ProgWrite v2 t2' =
  if v1 `leq` v2
    then t1' `isCoveredBy` t2'
    else do
      tell $ ppMismatch v1 v2
      return False
Stop `isCoveredBy` Stop = return True
_ `isCoveredBy` _ = tell "traces dont line up" >> return False

instance Normalizeable Int where
  toPattern n = ExactlyC n (show n)
  leq = S.isSubsetOf
  ppMismatch x y = ppOutputMismatch  (S.toList x) (S.toList y)

instance Normalizeable String where
  toPattern x = ExactlyC x x
  x `leq` y = x == y || all (\vs -> any (match vs) (S.toList y)) (S.toList x)
  ppMismatch xs ys = "Mismatch while comparing patterns: " ++ show (S.toList xs) ++ " is not covered by " ++ show (S.toList ys)

match :: [ConcretePattern String] -> [ConcretePattern String] -> Bool
--match xs ys | traceShow (xs,ys) False = undefined
match xs ys =
  length xs == length ys
  &&
  and (zipWith (\(ExactlyC v _) p -> check p v) xs ys)

instance (Show a) => Show (NTrace a) where
  show = showNTrace

instance (Show a) => Show (Trace' a a) where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

showNTrace :: Show a => NTrace a -> String
showNTrace (ProgRead v t) = "?"++show v++" "++showNTrace t
showNTrace (ProgWrite v t) =
  let vs = (\x -> if x == "[]" then "e" else x) . show <$> S.toList v
  in "!{"++ intercalate "," vs ++"} "++showNTrace t
showNTrace Stop = "stop"
showNTrace OutOfInputs = "<out of inputs>"

similar :: Eq a => Trace' f a -> Trace' g a -> Bool
similar x y = inputs x == inputs y

ppOutputMismatch :: Show a => [a] -> [a] -> String
ppOutputMismatch [v1] v2 =
  let clean :: Show a => a -> String
      clean = filter (`notElem` ['\\','\"']) . show
  in case v2 of
      [v2'] -> "Output mismatch: Expected " ++ clean v2' ++ ". But got " ++ clean v1
      _ ->  "Output mismatch: Expected one of " ++ clean v2 ++ ". But got " ++ clean v1
ppOutputMismatch _ _ = error "should not happen"

inputs :: Trace' f a -> [a]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
