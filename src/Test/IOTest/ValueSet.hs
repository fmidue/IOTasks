{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.ValueSet
  ( ValueSet
  , Value(..)
  , valueSet
  , valueOf
  , containsValue
  , elimValueSet
  , valueFromString
  , Extract(..)
  ) where

import Test.IOTest.Utils
import Test.IOTest.Pattern
import Test.IOTest.Value

import Data.Proxy
import Data.Dynamic
import Type.Reflection

import System.Random

import Text.PrettyPrint.HughesPJClass hiding ((<>))

data ValueSet where
  MkValueSet :: (Extract a b, DecMem a b, Typeable b, StringEmbedding b) => TypeRep b -> a -> ValueSet

valueSet :: (Extract a b, DecMem a b, Typeable b, StringEmbedding b) => a -> ValueSet
valueSet = MkValueSet typeRep

valueOf :: RandomGen g => ValueSet -> g -> Value
valueOf (MkValueSet r vs) gen = Value r $ extract gen vs

containsValue :: ValueSet -> Value -> Bool
containsValue (MkValueSet r vs) (Value r' v) =
  case r `eqTypeRep` r' of
    Just HRefl -> vs `contains` v
    Nothing -> False

valueFromString :: ValueSet -> String -> Value
valueFromString (MkValueSet (_ :: TypeRep a) _) str = Value typeRep (unpack @a str)

elimValueSet :: RandomGen g => ValueSet -> g -> (forall a b. (Extract a b, DecMem a b, Typeable b, StringEmbedding b) => Proxy a -> b -> c) -> c
elimValueSet (MkValueSet _ (xs :: ty)) gen f = f (Proxy @ty) $ extract gen xs

class Extract ts t | ts -> t where
  extract :: RandomGen g => g -> ts -> t

instance Extract [t] t where
  extract gen xs =
    let (i,_) = randomR (0,length xs - 1) gen
    in xs !! i

instance Extract Pattern String where
  extract gen p =
    let
      (gen1, gen2) = split gen
      lengths = randomRs (0,maxLength) gen1
      randomStrings = chunks lengths . filter (/='_') $ randomRs ('A','z') gen2
    in replaceWildCards (render $ pPrint p) randomStrings
      where maxLength = 10

class DecMem xs x | xs -> x where
  contains :: xs -> x -> Bool

instance Eq x => DecMem [x] x where
  contains = flip elem

instance DecMem Pattern String where
  contains p x = buildPattern x `isSubPatternOf` p

chunks :: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks _ [] = []
chunks (n:ns) xs = take n xs : chunks ns (drop n xs)

replaceWildCards :: String -> [String] -> String
replaceWildCards "" _ = ""
replaceWildCards xs [] = xs
replaceWildCards ('_':xs) (y:ys) = y ++ replaceWildCards xs ys
replaceWildCards (x:xs) ys = x : replaceWildCards xs ys

-- this is not very usefull for large types right now, since all this does is
-- brute force generate and test.
-- Extracting numbers does either yield very large absolute values or takes
-- extremly long if the predicate is very specific
-- Right now this is more of an example what is possible with this interface
instance Random a => Extract (a -> Bool) a where
  extract gen f =
    let as = randoms gen
    in head $ dropWhile (not . f) as

instance DecMem (a -> Bool) a where
  contains = id
