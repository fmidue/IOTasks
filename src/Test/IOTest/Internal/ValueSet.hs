{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Internal.ValueSet
  ( ValueSet(..)
  , valueOf
  , elimValueSet
  ) where

import Test.IOTest.Utils
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Environment

import Data.Proxy
import Data.Dynamic

import System.Random

import Text.PrettyPrint.HughesPJClass hiding ((<>))

data ValueSet where
  ValueSet :: (Extract a b, Typeable b, StringEmbedding s b) => Proxy s -> a -> ValueSet

elimValueSet :: RandomGen g => ValueSet -> g -> (forall s a b. (Extract a b, Typeable b, StringEmbedding s b) => Proxy s -> Proxy a -> b -> c) -> c
elimValueSet (ValueSet ps (xs :: ty)) gen f = f ps (Proxy @ty) $ extract gen xs

valueOf :: RandomGen g => ValueSet -> g -> Value
valueOf vs gen = elimValueSet vs gen (\ ps _ x -> Value ps x)

class Extract ts t | ts -> t where
  extract :: RandomGen g => g -> ts -> t

instance Extract [t] t where
  extract gen xs =
    let (i,_) = randomR (0,length xs - 1) gen
    in xs !! i

instance Extract LinearPattern String where
  extract gen p =
    let
      (gen1, gen2) = split gen
      lengths = randomRs (0,maxLength) gen1
      randomStrings = chunks lengths . filter (/='_') $ randomRs ('A','z') gen2
    in if not $ hasHoles p
        then replaceWildCards (render $ pPrint p) randomStrings
        else error "can't extract from a pattern with holes"
      where maxLength = 10

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
