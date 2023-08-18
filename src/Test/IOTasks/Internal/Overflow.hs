{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Test.IOTasks.Internal.Overflow (
  OverflowWarning(..),
  evalOverflow,
  OverflowTreatment(..), SubCheck(..),
  modifySubCheck,
  EffectEval(..),
  effectEval,
  I, fromInt,
  unwrapI, unwrapIs,
  ) where

import Control.Monad.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (second)
import Type.Reflection

import Type.Match

data OverflowWarning = OverflowOccurred | NoOverflow deriving (Eq,Show)

instance Semigroup OverflowWarning where
  NoOverflow <> o = o
  o <> NoOverflow = o
  OverflowOccurred <> OverflowOccurred = OverflowOccurred

instance Monoid OverflowWarning where
  mempty = NoOverflow

class EffectEval t where
  type Env t
  pureEval :: (Applicative f, Typeable a) => (forall x. Typeable x => t x -> f x) -> Env t -> t a -> f a
  eval :: Typeable a => Env t -> t a -> a
  eval d = runIdentity . effectEval (const Nothing) d

effectEval :: (EffectEval t, Applicative f, Typeable a) => (forall x. Typeable x => t x -> Maybe (f x)) -> Env t -> t a -> f a
effectEval f d x = fromMaybe (pureEval (effectEval f d) d x) $ f x


evalOverflow :: (EffectEval t, Typeable a) => OverflowTreatment t -> Env t -> t a -> (OverflowWarning, a)
evalOverflow = evalOverflow' where
  evalOverflow' :: forall t a. (EffectEval t, Typeable a) => OverflowTreatment t -> Env t -> t a -> (OverflowWarning, a)
  evalOverflow' OverflowTreatment{..} d = effectEval (effect d) d
    where
      effect :: forall a. Typeable a => Env t -> t a -> Maybe (OverflowWarning, a)
      effect d x = matchType @a
        [ inCaseOfE' @Integer $ \HRefl ->
          case evalITerm d x of
            Right i -> Just $ unwrapI i
            Left (SubCheck t f) -> do -- Maybe
              fx <- effect d t
              pure $ do -- (Overflow, a)
                x <- fx
                unwrapI $ f x
        , inCaseOfE' @[Integer] $ \HRefl ->
          case evalIList d x of
            Right i ->  Just $ unwrapIs i
            Left (SubCheck t f) -> do -- Maybe
              fx <- effect d t
              pure $ do -- (Overflow, a)
                x <- fx
                unwrapIs $ f x
        , fallbackCase' Nothing
        ]

data OverflowTreatment t = OverflowTreatment
  { evalITerm :: Env t -> t Integer   -> Either (SubCheck t I) I
  , evalIList :: Env t -> t [Integer] -> Either (SubCheck t [I]) [I]
  }

data SubCheck t x where
  SubCheck :: Typeable a => t a -> (a -> x) -> SubCheck t x

modifySubCheck :: (forall a. t a -> t' a) -> SubCheck t x -> SubCheck t' x
modifySubCheck n (SubCheck t f) = SubCheck (n t) f
---

data I = I Integer Int deriving (Eq,Ord)

fromInt :: Int -> I
fromInt x = I (toInteger x) x

instance Show I where
  show i@(I x _)
    | hasDiverged i = show x -- ++ "*"
    | otherwise = show x

hasDiverged :: I -> Bool
hasDiverged (I x x') = x /= fromIntegral x'

unwrapI :: I -> (OverflowWarning, Integer)
unwrapI i = (if hasDiverged i then OverflowOccurred else NoOverflow, toInteger i)

unwrapIs :: [I] -> (OverflowWarning, [Integer])
unwrapIs = foldMap (second pure . unwrapI)

instance Num I where
  (+) = liftOp2 (+) (+)
  (*) = liftOp2 (*) (*)
  (-) = liftOp2 (-) (-)
  abs = liftOp abs abs
  signum = liftOp signum signum
  fromInteger n = I n (fromInteger n)

liftOp :: (Integer -> Integer) -> (Int -> Int) -> I -> I
liftOp f g (I x x') = I (f x) (g x')

liftOp2 :: (Integer -> Integer -> Integer) -> (Int -> Int -> Int) -> I -> I -> I
liftOp2 f g (I x x') (I y y') = I (f x y) (g x' y')

instance Enum I where
  succ = liftOp succ succO
  pred = liftOp pred predO
  toEnum n = I (toEnum n) (toEnum n)
  fromEnum (I n _) = fromEnum n
  enumFrom x = x : enumFrom (succ x)
  enumFromThen x y = x : f y where
    s = y - x
    f v = v : f (v + s)
  enumFromTo x y =
    case compare x y of
      GT -> []
      EQ -> [x]
      LT -> x : enumFromTo (succ x) y
  enumFromThenTo x y z
    | z < x = []
    | otherwise = x : f y
    where
      s = y - x
      f v | v > z = []
          | otherwise = v : f (v+s)

instance Integral I where
  toInteger (I n _) = n
  quot = liftOp2 quot quot
  rem = liftOp2 rem rem
  div = liftOp2 div div
  mod = liftOp2 mod mod
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)

instance Real I where
  toRational (I n _) = toRational n

succO :: Int -> Int
succO x
  | x == maxBound = minBound
  | otherwise = succ x

predO :: Int -> Int
predO x
  | x == minBound = maxBound
  | otherwise = pred x
