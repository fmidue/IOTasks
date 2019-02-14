{-# LANGUAGE DeriveFunctor #-}
module Trace where

import Data.List
import           Data.Maybe

data Trace' a
  = ProgRead a (Trace' a)
  | ProgWrite [a] (Trace' a)
  | Stop
  | OutOfInputs
  deriving (Functor)

type Trace = Trace' Int

instance Show a => Show (Trace' a) where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

lessGeneralThan :: (Show a, Eq a) => Trace' a -> Trace' a -> Bool
lessGeneralThan t t' = isNothing $ lessGeneralThan' t t'

lessGeneralThan' :: (Show a, Eq a) => Trace' a -> Trace' a -> Maybe String
ProgRead v1 t1 `lessGeneralThan'` ProgRead v2 t2 =
  if v1 == v2 then t1 `lessGeneralThan'` t2 else Just "Traces don't line up"
ProgWrite v1 t1 `lessGeneralThan'` ProgWrite v2 t2 =
  if (v1 `union` v2) == v2
    then t1 `lessGeneralThan'` t2
    else Just $ ppOutputMismatch v1 v2
Stop `lessGeneralThan'` Stop = Nothing
_ `lessGeneralThan'` _ = Just "Traces don't line up"

ppOutputMismatch :: Show a => [a] -> [a] -> String
ppOutputMismatch [v1] [v2] = "Output mismatch: Expected " ++ show v2 ++ ". But got " ++ show v1
ppOutputMismatch [v1] v2 = "Output mismatch: Expected one of " ++ show v2 ++ ". But got " ++ show v1
ppOutputMismatch v1 v2 = "Output mismatch: Expected subset of " ++ show v2 ++ ". But got " ++ show v1

inputs :: Trace' a -> [a]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
