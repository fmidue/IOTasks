{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PolyKinds #-}
module Test.IOTasks.Terms (
  Varname,
  SomeVar, someVar, unSomeVar,
  pattern SomeVar,
  Var(..),
  var, intVar, stringVar,
  as,
  VarExp(..),
  varname, someVarname,
  varExpType,

  Accessor(..),
  Arithmetic(..),
  Compare(..),
  Logic(..),
  Sets(..),
  BasicLists(..),
  ComplexLists(..),
  Opaque(..),
  ) where

import Data.Function (on)
import Data.Bifunctor (second)

import Type.Reflection

type Varname = String

newtype Var (a :: k) = Var { unVar :: (Varname, TypeRep a) } deriving (Eq,Ord, Show)

data SomeVar where
  SomeVarC :: Typeable a => Var a -> SomeVar

{-# COMPLETE SomeVar #-}
pattern SomeVar :: (Varname, SomeTypeRep) -> SomeVar
pattern SomeVar x <- SomeVarC (Var (second SomeTypeRep -> x))
  where
    SomeVar (x,SomeTypeRep ty) = withTypeable ty $ SomeVarC $ Var (x,ty)

someVar :: Typeable a => Var a -> SomeVar
someVar = SomeVarC

unSomeVar :: SomeVar -> (Varname, SomeTypeRep)
unSomeVar (SomeVar x) = x


instance Eq SomeVar where
  (==) = (==) `on` unSomeVar
instance Ord SomeVar where
  compare = compare `on` unSomeVar
deriving instance Show SomeVar

varname :: Var a -> Varname
varname = fst . unVar

someVarname :: SomeVar -> Varname
someVarname = fst . unSomeVar

someVarType :: SomeVar -> SomeTypeRep
someVarType = snd . unSomeVar

varExpType :: VarExp e => e -> Maybe SomeTypeRep
varExpType = varListType . toVarList

varListType :: [SomeVar] -> Maybe SomeTypeRep
varListType xs =
  if same . map someVarType $ xs
    then Just $ someVarType . head $ xs
    else Nothing

same :: Eq a => [a] -> Bool
same xs = and $ zipWith (==) xs (tail xs)

var :: forall a. Typeable a => String -> Var a
var x = Var (x, typeRep)

intVar :: String -> Var Integer
intVar = var @Integer

stringVar :: String -> Var String
stringVar = var @String

class VarExp e where
  toVarList :: e -> [SomeVar]

instance VarExp SomeVar where
  toVarList = pure

instance Typeable a => VarExp (Var a) where
  toVarList = pure . someVar

instance VarExp [SomeVar] where
  toVarList = id

instance Typeable a => VarExp [Var a] where
  toVarList = map someVar

class Accessor t where
  currentValue :: (Typeable a, VarExp e) => e -> t a
  currentValue = valueBefore 0
  valueBefore:: (Typeable a, VarExp e) => Int -> e -> t a

  allValues :: (Typeable a, VarExp e) => e -> t [a]
  allValues = valuesBefore 0
  valuesBefore :: (Typeable a, VarExp e) => Int -> e -> t [a]

as :: Typeable a => t a -> t a
as = id

-- TODO: good names?
class Arithmetic t where
  (.+.) :: t Integer -> t Integer -> t Integer
  (.-.) :: t Integer -> t Integer -> t Integer
  (.*.) :: t Integer -> t Integer -> t Integer
  intLit :: Integer -> t Integer

class Compare t where
  (.==.) :: (Typeable a, Eq a) => t a -> t a -> t Bool
  (.>.) :: (Typeable a, Ord a) => t a -> t a -> t Bool
  (.>=.) :: (Typeable a, Ord a) => t a -> t a -> t Bool
  (.<.) :: (Typeable a, Ord a) => t a -> t a -> t Bool
  (.<=.) :: (Typeable a, Ord a) => t a -> t a -> t Bool

class Logic t where
  not' :: t Bool -> t Bool
  (.&&.) :: t Bool -> t Bool -> t Bool
  (.||.) :: t Bool -> t Bool -> t Bool
  true :: t Bool
  false :: t Bool

class BasicLists t where
  length' :: Typeable a => t [a] -> t Integer
  reverse' :: Typeable a => t [a] -> t [a]
  sum' :: t [Integer] -> t Integer
  product' :: t [Integer] -> t Integer
  listLit :: (Show a, Typeable a) => [a] -> t [a]

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

class Opaque t where
  liftOpaqueValue :: Typeable a => (a, String) -> t a
  liftOpaque2 :: (Typeable a, Typeable b) => (a -> b, String) -> t a -> t b
  liftOpaque3 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c, String) -> t a -> t b -> t c
