{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Test.IOTest.Internal.Environment
  ( Environment
  , update
  , freshEnvironment
  , lookupName
  , lookupNameAtType
  , HasVariables(..)
  , Varname
  , Value(..)
  , LookupError(..)
  , printLookupError
  ) where

import Test.IOTest.Utils

import Data.Dynamic
import Data.Proxy
import Type.Reflection

type Varname = String

type Environment = [ (Varname, Maybe (SomeTypeRep,[Value]) ) ]

data Value where
  Value :: (Typeable a, StringEmbedding s a) => Proxy s -> a -> Value

instance Show Value where
  show (Value p a) = pack p a

valueTypeRep :: Value -> SomeTypeRep
valueTypeRep (Value _ a) = dynTypeRep $ toDyn a

fromValue :: (Typeable a, StringEmbedding s a) => Proxy s -> Value -> Maybe a
fromValue _ (Value _ a) = fromDynamic (toDyn a)

update :: Varname -> Value -> Environment -> Maybe Environment
update x v = traverse (addValue x v)
  where
    addValue x' v' (y,Nothing) =
      if y == x'
        then Just (y, Just (valueTypeRep v', [v']))
        else Just (y,Nothing)
    addValue x' v' (y,Just (tyRep,vs')) =
      if y == x'
        then if tyRep == valueTypeRep v'
          then Just (y, Just (valueTypeRep v',vs' ++ [v']))
          else Nothing
        else Just (y,Just (tyRep,vs'))

freshEnvironment :: HasVariables a => a -> Environment
freshEnvironment s = (,Nothing) <$> vars s

data LookupError = NameNotFound String | WrongType String deriving Show

printLookupError :: LookupError -> String
printLookupError (NameNotFound e) = "lookup error: name not found: " <> e
printLookupError (WrongType e) = "lookup error: wrong type: " <> e

lookupName :: Varname -> Environment -> Either LookupError [Value]
lookupName x c =
  case lookup x c of
    Just Nothing -> Right []
    Just (Just (_, vs)) -> Right vs
    Nothing -> Left (NameNotFound $ x <> " in " <> show c)

lookupNameAtType :: (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Environment -> Either LookupError [a]
lookupNameAtType p x c =
  case lookupName x c of
    Left e -> Left e
    Right vs -> case traverse (fromValue p) vs of
      Just typedVs -> Right typedVs
      Nothing -> Left (WrongType $ x <> " in " <> show c)

class HasVariables a where
  vars :: a -> [Varname]
