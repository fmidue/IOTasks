{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.IOTasks.Language
  ( Specification, readInput, writeOutput, branch, tillExit, nop, exit
  , writeFixedOutput
  , Varname, optional
  , getCurrent, getAll
  , FixedPattern, buildPattern
  , TermPattern, buildTermPattern
  , Pattern (..), var, whitespace
  , ValueSet
  , intValues
  , values
  , stringValues
  , mkValueSet
  , ints, nats, integers
  , StringEmbedding
  , SpecTerm
  ) where

import Data.Dynamic (Typeable)

import Test.IOTasks.Utils
import Test.IOTasks.Specification
import Data.Term
import Data.Environment (Varname, Environment)
import Test.IOTasks.ValueSet
import Test.IOTasks.Pattern

import Test.QuickCheck (Gen, Arbitrary, elements)

type SpecTerm = ITerm Environment Varname

readInput :: Varname -> ValueSet -> Specification t
readInput x vs = Spec [ReadInput x vs]

writeOutput :: (Typeable a, StringEmbedding a) => [TermPattern] -> [t a] -> Specification t
writeOutput ps ts = Spec [WriteOutput False ps ts]

writeFixedOutput :: [TermPattern] -> Specification t
writeFixedOutput ps = Spec [WriteOutput False ps ([] :: [t String])]

branch :: t Bool -> Specification t -> Specification t -> Specification t
branch t s1 s2 = Spec [Branch t s1 s2]

tillExit :: Specification t -> Specification t
tillExit s = Spec [TillE s]

nop :: Specification t
nop = mempty

exit :: Specification t
exit = Spec [E]

getCurrent :: forall a t. (PVarTerm t Varname, Typeable a, Show a) => Varname -> t a
getCurrent = variableCurrent

getAll :: forall a t. (PVarTerm t Varname, Typeable a, Show a) => Varname -> t [a]
getAll = variableAll

intValues :: [Int] -> ValueSet
intValues = valueSet

values :: (Typeable a, Arbitrary a, StringEmbedding a, Eq a) => [a] -> ValueSet
values = valueSet

stringValues :: FixedPattern -> ValueSet
stringValues = valueSet

ints :: ValueSet
ints = valueSet' (const True) gen where
  gen :: Gen Int
  gen = elements [-10..10]

integers :: ValueSet
integers = valueSet' (const True) gen where
  gen :: Gen Integer
  gen = elements [-10..10]

nats :: ValueSet
nats = valueSet' (>= 0) gen where
  gen :: Gen Int
  gen = elements [0..10]

mkValueSet :: (Typeable a, Arbitrary a, StringEmbedding a) => (a -> Bool) -> Gen a -> ValueSet
mkValueSet = valueSet'
