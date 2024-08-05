{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.Var (
  Varname,
  SomeVar, someVar, unSomeVar,
  pattern SomeVar,
  Var(..),
  var, intVar, stringVar,
  varname, someVarname,
  VarExp(..),
  varExpType,
  ) where

import Data.Bifunctor ( second )
import Data.Function ( on )

import Type.Reflection
import Data.GADT.Compare
import Data.List (sort, nub)

type Varname = String

newtype Var (a :: k) = Var { unVar :: (Varname, TypeRep a) } deriving Show

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

instance Eq (Var a) where
  (==) = defaultEq

instance Ord (Var a) where
  compare = defaultCompare

instance GEq Var where
  geq x y = case gcompare x y of
    GEQ -> Just Refl
    _ -> Nothing

instance GCompare Var where
  gcompare (Var (x,tx)) (Var (y,ty)) =
    case compare x y of
      LT -> GLT
      EQ -> gcompare tx ty
      GT -> GGT

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

var :: Typeable a => Varname -> Var a
var x = Var (x, typeRep)

intVar :: Varname -> Var Integer
intVar = var @Integer

stringVar :: Varname -> Var String
stringVar = var @String

-- | Abstraction over different types of variable-expressions.
--
-- A variable-expression references one or more specification variables and
-- can be used to access the values assigned to these specific variables.
--
-- If an expression references more than one variable, the values of these
-- variables are interleaved chronologically. I.e., 'currentValue' will provide
-- access to the current value of the last modified variable from the list and
-- 'allValues' gives a list where values appear in the order they were given to the program.
--
-- Additionally, all referenced variables must be of the same type.
--
-- Instances need to make sure that the resulting list is sorted!
class VarExp e where
  toVarList :: e -> [SomeVar]
  -- ^ Computes the list of variables referenced by the variable expression.

instance VarExp SomeVar where
  toVarList = pure

instance Typeable a => VarExp (Var a) where
  toVarList = toVarList . someVar

instance VarExp [SomeVar] where
  toVarList = sort . nub

instance Typeable a => VarExp [Var a] where
  toVarList = toVarList . map someVar
