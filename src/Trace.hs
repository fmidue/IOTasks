{-# LANGUAGE DeriveFunctor #-}
module Trace where

import Data.Functor.Identity
import           Data.List (nub)
import Control.Monad.Trans.Writer

data Trace' f a
  = ProgRead a (Trace' f a)
  | ProgWrite (f a) (Trace' f a)
  | Stop
  | OutOfInputs
  deriving (Functor)

type IntTrace = Trace Int
type Trace a = Trace' Identity a
type GTrace a = Trace' OList a

data OList a
  = Must [a]
  | Can [a]
  deriving (Functor,Show)

union :: Eq a => OList a -> OList a -> OList a
union (Must xs) (Must ys) = Must $ nub $ xs ++ ys
union (Can xs) (Must ys) = Can $ nub $ xs ++ ys
union (Must xs) (Can ys) = Can $ nub $ xs ++ ys
union (Can xs) (Can ys) = Can $ nub $ xs ++ ys

instance (Show (f a), Show a) => Show (Trace' f a) where
  show (ProgRead v t) = "?"++show v++" "++show t
  show (ProgWrite v t) = "!"++show v++" "++show t
  show Stop = "stop"
  show OutOfInputs = "<out of inputs>"

similar :: Eq a => Trace' f a -> Trace' g a -> Bool
similar x y = inputs x == inputs y

covers :: (Show a, Eq a) => GTrace a -> Trace a -> Writer String Bool
ProgRead x tg `covers` ProgRead y t =
  if x == y
    then tg `covers` t
    else do
      tell $ "Input mismatch: Expected " ++ show x ++ ". But got " ++ show y
      return False
ProgWrite (Must xs) tg `covers` ProgWrite (Identity y) t =
  if y `elem` xs
    then tg `covers` t
    else do
      tell $ ppOutputMismatch y xs
      return False
ProgWrite (Can xs) tg `covers` t = do
  r1 <- ProgWrite (Must xs) tg `covers` t
  if r1 then return True else do
    r2 <- tg `covers` t
    return (r1 || r2)
Stop `covers` Stop = return True
_ `covers` _ = tell "traces dont line up" >> return False

ppOutputMismatch :: Show a => a -> [a] -> String
ppOutputMismatch v1 [v2] = "Output mismatch: Expected " ++ show v2 ++ ". But got " ++ show v1
ppOutputMismatch v1 v2 = "Output mismatch: Expected one of " ++ show v2 ++ ". But got " ++ show v1

inputs :: Trace' f a -> [a]
inputs (ProgRead v t) = v : inputs t
inputs (ProgWrite _ t) = inputs t
inputs Stop = []
inputs OutOfInputs = []
