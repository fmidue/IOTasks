{-# LANGUAGE FlexibleInstances #-}
module IOTasks.Terms where

type Varname = String

class VarExp a where
  toVarList :: a -> [Varname]

instance VarExp Varname where
  toVarList = pure

instance VarExp [Varname] where
  toVarList = id

class Accessor t where
  currentValue :: VarExp a => a -> t Integer
  allValues :: VarExp a => a -> t [Integer]

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

class BasicLists t where
  length' :: t [Integer] -> t Integer
  sum' :: t [Integer] -> t Integer
  product' :: t [Integer] -> t Integer

class ComplexLists t where
  -- TODO: improve signature?
  filter' :: (Integer -> Bool) -> t [Integer] -> t [Integer]
