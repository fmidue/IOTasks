{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Test.IOTasks.Var (
  Varname,
  Var(..), pattern Var,
  Embeddable(..), Embedded(..),
  SomeVar(..), someVar, someVarname, withSomeVar,
  SomeConsistentVars(..), someConsistentVars, withSomeConsistentVars,
  withConsistentList,
  filterType,
  intVar, stringVar, embeddedVar,
  varname, varTypeRep,
  VarExp(..),
  MergedVars, merge,
  ) where

import Type.Reflection
import Data.GADT.Compare
import Data.List (sort, nub)
import Data.Bifunctor (first)
import Type.Match (inCaseOfE', matchTypeOfMaybe)
import Data.Maybe (mapMaybe)

type Varname = String

data Var a where
  IntVar :: Varname -> Var Integer
  StringVar :: Varname -> Var String
  EmbeddedVar :: (Embeddable a, Show a, Read a) => TypeRep a -> Varname -> Var (Embedded a)

deriving instance Show (Var a)

-- | Values embeddable into a finite set of integers
--
-- Laws:
--
-- > asInteger . asOriginal = id
--
-- For types with Eq instances that induce non-trivial equivalence classes, at least the weaker
--
-- > asOriginal (asInteger x) == x = True
--
-- should hold. However program coverage is weakened in such scenarios, as generated inputs will only
-- ever contain values from the co-domain of 'asOriginal'.
class Embeddable a where
  asInteger :: a -> Integer
  asOriginal :: Integer -> a
  embeddingRange :: [Integer]

data Embedded a where
  Embedded :: (Show a, Embeddable a) => Integer -> Embedded a

deriving instance Eq (Embedded a)
instance Show a => Show (Embedded a) where
  show (Embedded i) = "Embedded (asInteger "++ show @a (asOriginal i) ++ ")"

instance (Show a, Read a, Embeddable a) => Read (Embedded a) where
  readsPrec p x = first (Embedded . asInteger)  <$> readsPrec @a p x

{-# COMPLETE Var #-}
pattern Var :: (Varname, TypeRep a) -> Var a
pattern Var x <- ((\v -> (varname v, varTypeRep v)) -> x)

data SomeVar where
  SomeVar :: Typeable a => Var a -> SomeVar

someVar :: Typeable a => Var a -> SomeVar
someVar = SomeVar

withSomeVar :: SomeVar -> (forall a. Typeable a => Var a -> r) -> r
withSomeVar (SomeVar x) f = f x

withConsistentList :: [SomeVar] -> (forall a. Typeable a => [Var a] -> r) -> Maybe r
withConsistentList xs f = do
  same <- someConsistentVars xs
  pure $ withSomeConsistentVars same f

data SomeConsistentVars where
  SomeConsistentVars :: Typeable a => [Var a] -> SomeConsistentVars

withSomeConsistentVars :: SomeConsistentVars -> (forall a. Typeable a => [Var a] -> r) -> r
withSomeConsistentVars (SomeConsistentVars xs) f = f xs

someConsistentVars :: [SomeVar] -> Maybe SomeConsistentVars
someConsistentVars [] = Nothing
someConsistentVars (x:xs) = foldr (\y mys -> addSome y =<< mys) (withSomeVar x (Just . SomeConsistentVars . pure)) xs

addSome :: SomeVar -> SomeConsistentVars -> Maybe SomeConsistentVars
addSome (SomeVar (x :: Var a)) (SomeConsistentVars (ys :: [Var b])) =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> Just $ SomeConsistentVars (x:ys)
    Nothing -> Nothing

filterType :: forall a. Typeable a => [SomeVar] -> [Var a]
filterType = mapMaybe (\(SomeVar x) -> matchTypeOfMaybe x [inCaseOfE' @(Var a) $ \HRefl -> x])

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
  x == y = compare x y == EQ
instance Ord SomeVar where
  compare (SomeVar x) (SomeVar y) = case gcompare x y of
    GLT -> LT
    GEQ -> EQ
    GGT -> GT
deriving instance Show SomeVar

varname :: Var a -> Varname
varname (IntVar x) = x
varname (StringVar x) = x
varname (EmbeddedVar _ x) = x

varTypeRep :: Var a -> TypeRep a
varTypeRep IntVar{} = typeRep
varTypeRep StringVar{} = typeRep
varTypeRep (EmbeddedVar ty _) = App (typeRep @Embedded) ty

someVarname :: SomeVar -> Varname
someVarname (SomeVar x) = varname x

intVar :: Varname -> Var Integer
intVar = IntVar

stringVar :: Varname -> Var String
stringVar = StringVar

embeddedVar :: (Typeable a, Embeddable a, Show a, Read a) => Varname -> Var (Embedded a)
embeddedVar = EmbeddedVar typeRep

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
  toVarList :: e a -> [Var a]
  -- ^ Computes the list of variables referenced by the variable expression.

instance VarExp Var where
  toVarList = pure

newtype MergedVars a = MergedVars { mergedVars :: [Var a] }
  deriving (Eq, Show)

merge :: [Var a] -> MergedVars a
merge = MergedVars

instance VarExp MergedVars where
  toVarList = sort . nub . mergedVars

