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
  currentValue :: VarExp a => a -> t I
  currentValue x = currentValue' x 0
  currentValue' :: VarExp a => a -> Int -> t I

  allValues :: VarExp a => a -> t [I]
  allValues x = allValues' x 0
  allValues' :: VarExp a => a -> Int -> t [I]

-- TODO: good names?
class Arithmetic t where
  (.+.) :: t I -> t I -> t I
  (.-.) :: t I -> t I -> t I
  (.*.) :: t I -> t I -> t I
  intLit :: Integer -> t I

class Compare t where
  (.==.) :: t I -> t I -> t Bool
  (.>.) :: t I -> t I -> t Bool
  (.>=.) :: t I -> t I -> t Bool
  (.<.) :: t I -> t I -> t Bool
  (.<=.) :: t I -> t I -> t Bool

class Logic t where
  not' :: t Bool -> t Bool
  (.&&.) :: t Bool -> t Bool -> t Bool
  (.||.) :: t Bool -> t Bool -> t Bool
  true :: t Bool
  false :: t Bool

class BasicLists t where
  length' :: t [I] -> t I
  sum' :: t [I] -> t I
  product' :: t [I] -> t I
  listLit :: [Integer] -> t [I]

class Sets t where
  isIn :: t I -> t [I] -> t Bool
  isNotIn :: t I -> t [I] -> t Bool

  default isIn :: Logic t => t I -> t [I] -> t Bool
  isIn x xs = not' (x `isNotIn` xs)
  default isNotIn :: Logic t => t I -> t [I] -> t Bool
  isNotIn x xs = not' (x `isIn` xs)

class ComplexLists t where
  -- TODO: improve signature?
  filter' :: (I -> Bool) -> t [I] -> t [I]
