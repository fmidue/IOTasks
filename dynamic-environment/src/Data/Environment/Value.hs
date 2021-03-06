{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Environment.Value where

import Data.Proxy

import Type.Reflection

import Unsafe.Coerce

data Value where
  Value :: Typeable a => TypeRep a -> (a -> String) -> a -> Value

value :: Typeable a => a -> (a -> String) -> Value
value x f = Value typeRep f x

extractAtType :: Typeable a => Proxy a -> Value -> Maybe a
extractAtType p v = fst <$> extractAtTypeWithShow p v

extractAtTypeWithShow :: Typeable a => Proxy a -> Value -> Maybe (a, a -> String)
extractAtTypeWithShow (Proxy :: Proxy a) (Value tyRep sho x) =
  case typeRep @a `eqTypeRep` tyRep of
    Nothing -> Nothing
    Just HRefl -> Just (x, sho)

-- skip the type comparison if known for some reason
unsafeExtract :: Value -> a
unsafeExtract (Value _ _ x) = unsafeCoerce x

instance Show Value where
  show (Value _ f a) = f a
