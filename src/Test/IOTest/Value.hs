{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTest.Value where

import Test.IOTest.Utils

import Data.Proxy

import Type.Reflection

data Value where
  Value :: (Typeable a, StringEmbedding a) => TypeRep a -> a -> Value

fromValue :: Typeable a => Proxy a -> Value -> Maybe a
fromValue (_ :: Proxy a) (Value r v) =
  case typeRep @a `eqTypeRep` r of
    Just HRefl -> Just v
    Nothing -> Nothing

instance Show Value where
  show (Value _ a) = pack a
