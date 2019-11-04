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

readInput :: Varname -> ValueSet -> Specification
readInput x vs = Spec [ReadInput x vs]

writeOutput :: StringEmbedding a => [TermPattern] -> [Term a] -> Specification
writeOutput ps ts = Spec [WriteOutput False ps ts]

writeFixedOutput :: [TermPattern] -> Specification
writeFixedOutput ps = Spec [WriteOutput False ps ([] :: [Term String])]

branch :: Term Bool -> Specification -> Specification -> Specification
branch t s1 s2 = Spec [Branch t s1 s2]

tillExit :: Specification -> Specification
tillExit s = Spec [TillE s]

nop :: Specification
nop = mempty

exit :: Specification
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

getCurrent :: (Typeable a, StringEmbedding a) => Varname -> Term a
getCurrent = T.getCurrent

getAll :: (Typeable a, StringEmbedding a) => Varname -> Term [a]
getAll = T.getAll
