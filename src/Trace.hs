{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Trace where

import Control.Monad.Trans.Writer

import Data.Bifunctor
import Data.Set as Set
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
type NTrace a = Trace' (Set [a]) a

normalize :: Trace a -> NTrace a
normalize = go [] where
  go w (ProgWrite v t') = go (w ++ [v]) t'
  go w (ProgRead v t') = ProgWrite (singleton w) $ ProgRead v $ go [] t'
  go w Stop = ProgWrite (singleton w) Stop
  go w OutOfInputs = ProgWrite (singleton w) OutOfInputs

instance (Show a) => Show (Trace' a a) where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

instance (Show a) => Show (Trace' (Set [a]) a) where
  show = showNTrace

showNTrace :: Show a => NTrace a -> String
showNTrace (ProgRead v t) = "?"++show v++" "++showNTrace t
showNTrace (ProgWrite v t) = let vs = (\x -> if x == "[]" then "e" else x). show <$> toList v in "!{"++ intercalate "," vs ++"} "++showNTrace t
showNTrace Stop = "stop"
showNTrace OutOfInputs = "<out of inputs>"

similar :: Eq a => Trace' f a -> Trace' g a -> Bool
similar x y = inputs x == inputs y

isCoveredBy :: (Show a, Ord a) => NTrace a -> NTrace a -> Writer String Bool
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
      tell $ ppOutputMismatch (toList v1) (toList v2)
      return False
Stop `isCoveredBy` Stop = return True
_ `isCoveredBy` _ = tell "traces dont line up" >> return False

ppOutputMismatch :: Show a => [a] -> [a] -> String
ppOutputMismatch v1 [v2] = "Output mismatch: Expected " ++ show v2 ++ ". But got " ++ show v1
ppOutputMismatch v1 v2 = "Output mismatch: Expected one of " ++ show v2 ++ ". But got " ++ show v1

inputs :: Trace' f a -> [a]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
