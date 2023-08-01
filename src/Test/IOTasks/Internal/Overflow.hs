{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Test.IOTasks.Internal.Overflow (
  OverflowWarning(..),
  OverflowType(..),
  I,
  ) where

import Type.Reflection

data OverflowWarning = OverflowOccurred | NoOverflow deriving (Eq,Show)

instance Semigroup OverflowWarning where
  NoOverflow <> o = o
  o <> NoOverflow = o
  OverflowOccurred <> OverflowOccurred = OverflowOccurred

instance Monoid OverflowWarning where
  mempty = NoOverflow

class (Typeable a, Show a, Typeable (OT a), Eq (OT a), Ord (OT a)) => OverflowType a where
  type OT a = r | r -> a
  checkOverflow :: OT a -> OverflowWarning

  toOT :: a -> OT a
  fromOT :: OT a -> a

  showOT :: OT a -> String
  default showOT :: Show (OT a) => OT a -> String
  showOT = show

instance OverflowType Integer where
  type OT Integer = I
  checkOverflow x
    | hasDiverged x = OverflowOccurred
    | otherwise = NoOverflow

  toOT = fromInteger
  fromOT = fromIntegral
  showOT = show @Integer . fromIntegral

instance OverflowType Bool where
  type OT Bool = Bool
  checkOverflow = const NoOverflow

  toOT = id
  fromOT = id
  showOT = show

instance OverflowType Char where
  type OT Char = Char
  checkOverflow = const NoOverflow

  toOT = id
  fromOT = id
  showOT = show

instance OverflowType a => OverflowType [a] where
  type OT [a] = [OT a]
  checkOverflow = foldMap checkOverflow

  toOT = map toOT
  fromOT = map fromOT
  showOT = show . map showOT

data I = I Integer Int deriving (Eq,Ord)

instance Show I where
  show i@(I x _)
    | hasDiverged i = show x -- ++ "*"
    | otherwise = show x

hasDiverged :: I -> Bool
hasDiverged (I x x') = x /= fromIntegral x'

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
