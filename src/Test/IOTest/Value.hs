{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTest.Value where

import Test.IOTest.Utils

import Type.Reflection

data Value where
  Value :: (Typeable a, StringEmbedding a) => TypeRep a -> a -> Value

instance Show Value where
  show (Value _ a) = pack a
