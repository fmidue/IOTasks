{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module IOTasks.Terms where

import IOTasks.Overflow

type Varname = String

class VarExp a where
  toVarList :: a -> [Varname]

instance VarExp Varname where
  toVarList = pure

instance VarExp [Varname] where
  toVarList = id

class Accessor t where
  currentValue :: VarExp a => a -> t Integer
  currentValue x = currentValue' x 0
  currentValue' :: VarExp a => a -> Int -> t Integer

  allValues :: VarExp a => a -> t [Integer]
  allValues x = allValues' x 0
  allValues' :: VarExp a => a -> Int -> t [Integer]

-- TODO: good names?
class Arithmetic t where
  (.+.) :: t Integer -> t Integer -> t Integer
  (.-.) :: t Integer -> t Integer -> t Integer
  (.*.) :: t Integer -> t Integer -> t Integer
  intLit :: Integer -> t Integer

class Compare t where
  (.==.) :: t Integer -> t Integer -> t Bool
  (.>.) :: t Integer -> t Integer -> t Bool
  (.>=.) :: t Integer -> t Integer -> t Bool
  (.<.) :: t Integer -> t Integer -> t Bool
  (.<=.) :: t Integer -> t Integer -> t Bool

class Logic t where
  not' :: t Bool -> t Bool
  (.&&.) :: t Bool -> t Bool -> t Bool
  (.||.) :: t Bool -> t Bool -> t Bool
  true :: t Bool
  false :: t Bool

class BasicLists t where
  length' :: t [Integer] -> t Integer
  sum' :: t [Integer] -> t Integer
  product' :: t [Integer] -> t Integer
  listLit :: [Integer] -> t [Integer]

class Sets t where
  isIn :: t Integer -> t [Integer] -> t Bool
  isNotIn :: t Integer -> t [Integer] -> t Bool

  default isIn :: Logic t => t Integer -> t [Integer] -> t Bool
  isIn x xs = not' (x `isNotIn` xs)
  default isNotIn :: Logic t => t Integer -> t [Integer] -> t Bool
  isNotIn x xs = not' (x `isIn` xs)

class ComplexLists t where
  -- TODO: improve signature?
  filter' :: (Integer -> Bool) -> t [Integer] -> t [Integer]
