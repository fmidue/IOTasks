{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTasks.Value where

import Test.IOTasks.Utils

import Type.Reflection

data Value where
  Value :: (Typeable a, StringEmbedding a) => TypeRep a -> a -> Value

instance Show Value where
  show (Value _ a) = pack a
