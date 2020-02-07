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
  , valueSet'
  , valueOf
  , containsValue
  , withProxy
  , valueFromString
  , Extract(..)
  ) where

import Test.IOTest.Utils
import Test.IOTest.Pattern
import Test.IOTest.Value

import Data.Proxy
import Data.Dynamic
import Type.Reflection

import Text.PrettyPrint.HughesPJClass hiding ((<>))
import Test.QuickCheck (Gen, Arbitrary, arbitrary, arbitraryPrintableChar)
import Test.QuickCheck.GenT

data ValueSet where
  MkValueSet :: (Typeable a, Arbitrary a, StringEmbedding a) =>
        { _containedType :: TypeRep a
        , _isMember :: a -> Bool
        , _valueGen :: Gen a
        } -> ValueSet

valueSet :: (Extract t a, DecMem t a, Typeable a, Arbitrary a, StringEmbedding a) => t -> ValueSet
valueSet x =  valueSet' (x `contains`) (extract x)

valueSet' :: (Typeable a, Arbitrary a, StringEmbedding a) => (a -> Bool) -> Gen a -> ValueSet
valueSet' = MkValueSet typeRep

valueOf :: MonadGen m => ValueSet -> m Value
valueOf (MkValueSet r _ gen) = liftGen $ Value r <$> gen

containsValue :: ValueSet -> Value -> Bool
containsValue (MkValueSet r p _) (Value r' v) =
  case r `eqTypeRep` r' of
    Just HRefl -> p v
    Nothing -> False

valueFromString :: ValueSet -> String -> Value
valueFromString (MkValueSet (_ :: TypeRep a) _ _) str = Value typeRep (unpack @a str)

withProxy :: ValueSet -> (forall a. (Typeable a, Arbitrary a, StringEmbedding a) => Proxy a -> b) -> b
withProxy (MkValueSet (_ :: TypeRep a) _ _) f = f (Proxy @a)

class Extract ts t | ts -> t where
  extract :: MonadGen m => ts -> m t

instance Extract [t] t where
  extract = elements

instance Extract Pattern String where
  extract p = do
    randomStrings <- listOf $ filter (/='_') <$> liftGen (listOf arbitraryPrintableChar)
    return $ replaceWildCards (unescapeNewline $ render $ pPrint p) randomStrings

unescapeNewline :: String -> String
unescapeNewline "" = ""
unescapeNewline ('\\':'n':xs) = '\n' : unescapeNewline xs
unescapeNewline (x:xs) = x : unescapeNewline xs

class DecMem xs x | xs -> x where
  contains :: xs -> x -> Bool

instance Eq x => DecMem [x] x where
  contains = flip elem

instance DecMem Pattern String where
  contains p x = buildPattern x `isSubPatternOf` p

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
instance Arbitrary a => Extract (a -> Bool) a where
  extract f =  liftGen $ arbitrary `suchThat` f

instance DecMem (a -> Bool) a where
  contains = id
