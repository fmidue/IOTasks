{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Test.IOTest.Internal.Context
  ( Context
  , update
  , freshContext
  , lookupName
  , lookupNameAtType
  , HasVariables(..)
  , Varname
  , Value(..)
  ) where

import Test.IOTest.Utils

import Data.Dynamic
import Data.Proxy
import Type.Reflection

type Varname = String

type Context = [(Varname,(SomeTypeRep,[Value]))]

data Value where
  Value :: (Typeable a, StringEmbedding s a) => Proxy s -> a -> Value

instance Show Value where
  show (Value p a) = pack p a

valueTypeRep :: Value -> SomeTypeRep
valueTypeRep (Value _ a) = dynTypeRep $ toDyn a

fromValue :: (Typeable a, StringEmbedding s a) => Proxy s -> Value -> Maybe a
fromValue _ (Value _ a) = fromDynamic (toDyn a)

update :: Context -> Varname -> Value -> Maybe Context
update vs x v = traverse (addValue x v) vs
  where
    addValue x' v' (y,(tyRep,vs')) =
      if y == x'
        then if null vs' || tyRep == valueTypeRep v'
          then Just (y,(valueTypeRep v',vs' ++ [v']))
          else Nothing
        else Just (y,(tyRep,vs'))

freshContext :: HasVariables a => a -> Context
freshContext s = (,(undefined,[])) <$> vars s

lookupName :: Varname -> Context -> Maybe [Value]
lookupName = fmap snd ... lookup

lookupNameAtType :: (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Context -> Maybe [a]
lookupNameAtType p x c = do
  vs <- snd <$> lookup x c
  traverse (fromValue p) vs

class HasVariables a where
  vars :: a -> [Varname]
