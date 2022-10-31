{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IOTasks.Overflow where

import Data.Constraint

import Type.Reflection
import Data.Void

data OverflowWarning = OverflowWarning | NoOverflow deriving (Eq,Show)

instance Semigroup OverflowWarning where
  NoOverflow <> o = o
  o <> NoOverflow = o
  OverflowWarning <> OverflowWarning = OverflowWarning

instance Monoid OverflowWarning where
  mempty = NoOverflow

-- Overflow detection type
class Typeable a => OverflowType a where
  type OT a = r | r -> a
  typeRepT :: TypeRep (OT a)
  otEqDict :: Dict (Eq (OT a))
  otOrdDict :: Dict (Ord (OT a))
  type InnerOT a
  innerDict :: Maybe (Dict (OverflowType (InnerOT a)))

instance OverflowType Integer where
  type OT Integer = I
  typeRepT = typeRep
  otEqDict = Dict
  otOrdDict = Dict
  type InnerOT Integer = Void
  innerDict = Nothing

instance OverflowType Bool where
  type OT Bool = Bool
  typeRepT = typeRep
  otEqDict = Dict
  otOrdDict = Dict
  type InnerOT Bool = Void
  innerDict = Nothing

instance OverflowType Char where
  type OT Char = Char
  typeRepT = typeRep
  otEqDict = Dict
  otOrdDict = Dict
  type InnerOT Char = Void
  innerDict = Nothing

instance OverflowType a => OverflowType [a] where
  type OT [a] = [OT a]
  otEqDict = case otEqDict @a of Dict -> Dict
  otOrdDict = case otOrdDict @a of Dict -> Dict
  typeRepT = withTypeable (typeRepT @a) typeRep
  type InnerOT [a] = a
  innerDict = Just Dict

data I = I Integer Int deriving (Eq,Ord)

instance Show I where
  show i@(I x _)
    | hasDiverged i = show x -- ++ "*"
    | otherwise = show x

hasDiverged :: I -> Bool
hasDiverged (I x x') = x /= fromIntegral x'

checkOverflow :: I -> OverflowWarning
checkOverflow x
  | hasDiverged x = OverflowWarning
  | otherwise = NoOverflow

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
