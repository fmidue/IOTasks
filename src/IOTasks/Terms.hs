{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.Terms where

import IOTasks.Overflow

import Data.Typeable

type Varname = String

type Var = (Varname, TypeRep)

varname :: Var -> Varname
varname = fst

varExpType :: VarExp e => e -> Maybe TypeRep
varExpType = varListType . toVarList

varListType :: [Var] -> Maybe TypeRep
varListType xs =
  if same . map snd $ xs
    then Just $ snd . head $ xs
    else Nothing

same :: Eq a => [a] -> Bool
same xs = and $ zipWith (==) xs (tail xs)

var :: forall a. Typeable a => String -> Var
var x = (x, typeRep $ Proxy @a)

intVar :: String -> Var
intVar = var @Integer

stringVar :: String -> Var
stringVar = var @String

class VarExp e where
  toVarList :: e -> [Var]

instance VarExp Var where
  toVarList = pure

instance VarExp [Var] where
  toVarList = id

class Accessor t where
  currentValue :: (OverflowType a, VarExp e) => e -> t a
  currentValue x = currentValue' x 0
  currentValue' :: (OverflowType a, VarExp e) => e -> Int -> t a

  allValues :: (OverflowType a, VarExp e) => e -> t [a]
  allValues x = allValues' x 0
  allValues' :: (OverflowType a, VarExp e) => e -> Int -> t [a]

as :: Typeable a => t a -> t a
as = id

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
  length' :: Typeable a => t [a] -> t Integer
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
