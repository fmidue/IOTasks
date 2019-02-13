{-# LANGUAGE DeriveFunctor #-}
module Trace where

import Data.List

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

lessGeneralThan :: Eq a => Trace' a -> Trace' a -> Bool
ProgRead v1 t1 `lessGeneralThan` ProgRead v2 t2 = v1 == v2 && t1 `lessGeneralThan` t2
ProgWrite v1 t1 `lessGeneralThan` ProgWrite v2 t2 = (v1 `union` v2) == v2  && t1 `lessGeneralThan` t2
Stop `lessGeneralThan` Stop = True
_ `lessGeneralThan` _ = False

inputs :: Trace' a -> [a]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
