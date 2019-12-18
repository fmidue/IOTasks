{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.IOTest.Language
  ( Specification, readInput, writeOutput, branch, tillExit, nop, exit
  , writeFixedOutput
  , Varname, Term, optional
  , getCurrent, getAll
  , Pattern, buildPattern
  , TermPattern, buildTermPattern
  , intValues
  , ints
  , nats
  ) where

import Test.IOTest.Utils
import Test.IOTest.Specification
import qualified Test.IOTest.Term as T
import Test.IOTest.Term (Term)
import Test.IOTest.Environment (Varname)
import Test.IOTest.ValueSet
import Test.IOTest.Pattern

import Data.Typeable

import Test.QuickCheck (Gen, elements)

readInput :: Varname -> ValueSet -> Specification t
readInput x vs = Spec [ReadInput x vs]

writeOutput :: StringEmbedding a => [TermPattern] -> [t a] -> Specification t
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

intValues :: [Int] -> ValueSet
intValues = valueSet

ints :: ValueSet
ints = valueSet' (const True) gen where
  gen :: Gen Int
  gen = elements [-10..10]

nats :: ValueSet
nats = valueSet' (>= 0) gen where
  gen :: Gen Int
  gen = elements [0..10]

getCurrent :: forall a t. (Typeable a, StringEmbedding a, Term t) => Varname -> t a
getCurrent = T.getCurrent

getAll :: forall a t. (Typeable a, StringEmbedding a, Term t) => Varname -> t [a]
getAll = T.getAll
