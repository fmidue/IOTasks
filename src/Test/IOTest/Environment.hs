{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Test.IOTest.Environment
  ( Environment
  , update
  , freshEnvironment
  , lookupNameAtType
  , HasVariables(..)
  , Varname
  , Value(..)
  , printLookupError
  ) where

import Test.IOTest.Utils

import Data.Dynamic
import Type.Reflection

type Varname = String

type Environment = [ (Varname, Maybe (SomeTypeRep,[Value]) ) ]

data Value where
  Value :: (Typeable a, StringEmbedding a) => a -> Value

instance Show Value where
  show (Value a) = pack a

valueTypeRep :: Value -> SomeTypeRep
valueTypeRep (Value a) = dynTypeRep $ toDyn a

fromValue :: Typeable a => Value -> Maybe a
fromValue (Value a) = fromDynamic (toDyn a)

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
lookupName x c = snd <$> lookupName' x c

lookupName' :: Varname -> Environment -> Either LookupError (SomeTypeRep,[Value])
lookupName' x c =
  case lookup x c of
    Just Nothing -> Right (undefined,[])
    Just (Just (tyRep, vs)) -> Right (tyRep, vs)
    Nothing -> Left (NameNotFound $ x <> " in " <> show c)

lookupNameAtType :: Typeable a => Varname -> Environment -> Either LookupError [a]
lookupNameAtType x c =
  case lookupName x c of
    Left e -> Left e
    Right vs -> case traverse fromValue vs of
      Just typedVs -> Right typedVs
      Nothing -> Left (WrongType $ x <> " in " <> show c)

class HasVariables a where
  vars :: a -> [Varname]
