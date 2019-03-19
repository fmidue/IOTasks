{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.IOProperty (
  IOProperty,
  fulfills,
  fulfillsNot,
) where

import Test.IOTest.IOtt (IOtt, runProgram)
import Test.IOTest.Internal.Specification
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.TraceSet
import Test.IOTest.Utils

import Data.Coerce
import Data.List as List

import Control.Arrow ((&&&))
import Control.Monad.Trans.Writer.Lazy
import Test.QuickCheck
import Data.Bifunctor (bimap)

import qualified Data.Set as Set

data IOProperty prog spec = Fulfills prog spec | FulfillsNot prog spec

fulfills :: a -> b -> IOProperty a b
fulfills = Fulfills
fulfillsNot :: a -> b -> IOProperty a b
fulfillsNot = FulfillsNot

instance (Eq a, Show a, Ord a, StringRep a, Normalizeable a, Restrictable a) => Testable (IOProperty (IOtt ()) (Specification VarName a)) where
  property (prog `Fulfills` spec) = specProperty spec prog
  property (prog `FulfillsNot` spec) = expectFailure $ specProperty spec prog

instance (Show b, Arbitrary b, Testable (IOProperty a' b'), Coercible b a) => Testable (IOProperty (a -> a') (b -> b')) where
  property (Fulfills f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `Fulfills` g x)
  property (FulfillsNot f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `FulfillsNot` g x)

specProperty :: (Eq a, StringRep a, Show a, Ord a, Normalizeable a, Restrictable a) => Specification VarName a -> IOtt () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAll gen prop

testTrace :: (Eq a, Show a, StringRep a, Normalizeable a) => (NTrace a, [a]) -> IOtt () -> Property
testTrace (tg,i) p =
  let trace = runProgram (to <$> i) p -- TODO: conversion problem?
      normString = normalize trace
      normA = normalize . bimap from from $ trace
      w = normA `isCoveredBy` tg
      (result,msg) = runWriter w
  in counterexample (msg ++ "\n  program trace: " ++ show (mapNTrace Wrap normString)) result

mapNTrace :: Ord b => (a -> b) -> NTrace a -> NTrace b
mapNTrace f = bimap (Set.map (List.map (fmap f))) f

newtype StringWrapper = Wrap String deriving (Eq,Ord)
instance Show StringWrapper where
  show (Wrap s) = s
