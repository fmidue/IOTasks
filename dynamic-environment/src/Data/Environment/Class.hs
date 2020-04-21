{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Environment.Class (
  VarEnv(..),
  store,
  lookupAtType,
  lookupAtTypeWithShow,
  PVarEnv(..),
  lookupAllAtType,
  lookupAllAtTypeWithShow,
  lookupLastAtType,
  lookupLastAtTypeWithShow,
  LookupError(..),
  printLookupError,
  ) where

import Data.Proxy
import Data.Environment.Value

import Data.List (intercalate)

import Type.Reflection

-- | mapping of names to (dynamically) typed values
class VarEnv env v where
  storeValue :: v -> Value -> env v -> Maybe (env v)
  lookupValue :: v -> env v -> Either (LookupError v) Value
  freshEnvironment :: [v] -> env v

  emptyEnvironment :: env v
  emptyEnvironment = freshEnvironment []

store :: (VarEnv env v, Typeable a) => v -> (a -> String) -> a -> env v -> Maybe (env v)
store v f x = storeValue v (value x f)

lookupAtType :: (VarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) a
lookupAtType p x e = fst <$> lookupAtTypeWithShow p x e

lookupAtTypeWithShow :: (VarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) (a, a -> String)
lookupAtTypeWithShow p x e = case lookupValue x e of
  Left err -> Left err
  Right v -> case extractAtTypeWithShow p v of
    Just a -> Right a
    Nothing -> Left $ WrongType x

-- | persistent variable environments
class VarEnv env v => PVarEnv env v where
  lookupAll :: v -> env v -> Either (LookupError v) [Value]

lookupAllAtType :: (PVarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) [a]
lookupAllAtType p x e = fst <$> lookupAllAtTypeWithShow p x e

lookupAllAtTypeWithShow :: (PVarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) ([a], [a] -> String)
lookupAllAtTypeWithShow p x e = case lookupAll x e of
  Left err -> Left err
  Right vs -> case traverse (extractAtTypeWithShow p) vs of
    Just vs' -> Right (map fst vs',\xs -> "[" ++ intercalate "," (zipWith snd vs' xs) ++ "]")
    Nothing -> Left $ WrongType x

-- | lookupAtType alias for persistent environments
lookupLastAtType :: (PVarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) a
lookupLastAtType = lookupAtType

lookupLastAtTypeWithShow :: (PVarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) (a, a -> String)
lookupLastAtTypeWithShow = lookupAtTypeWithShow

data LookupError v = NameNotFound v | WrongType v | NoValuePresent v deriving Show

printLookupError :: Show v => LookupError v -> String
printLookupError (NameNotFound v) = "lookup error: name not found: " <> show v
printLookupError (WrongType v) = "lookup error: wrong type: " <> show v
printLookupError (NoValuePresent v) = "lookup error: no value present: " <> show v
