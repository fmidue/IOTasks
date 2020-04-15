{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Environment.Class (
  VarEnv(..),
  store,
  PVarEnv(..),
  lookupLastAtType,
  LookupError(..),
  printLookupError,
  ) where

import Data.Proxy
import Data.Environment.Value

import Type.Reflection

-- | mapping of names to (dynamically) typed values
class VarEnv env v where
  storeValue :: v -> Value -> env v -> Maybe (env v)
  lookupAtType :: Typeable a => Proxy a -> v -> env v -> Either (LookupError v) a
  freshEnvironment :: [v] -> env v

  emptyEnvironment :: env v
  emptyEnvironment = freshEnvironment []

store :: (VarEnv env v, Typeable a) => v -> (a -> String) -> a -> env v -> Maybe (env v)
store v f x = storeValue v (value x f)

-- | persistent variable environments
class VarEnv env v => PVarEnv env v where
  lookupAllAtType :: Typeable a => Proxy a -> v -> env v -> Either (LookupError v) [a]

-- | lookupAtType alias for persistent environments
lookupLastAtType :: (PVarEnv env v, Typeable a) => Proxy a -> v -> env v -> Either (LookupError v) a
lookupLastAtType = lookupAtType

data LookupError v = NameNotFound v | WrongType v | NoValuePresent v deriving Show

printLookupError :: Show v => LookupError v -> String
printLookupError (NameNotFound v) = "lookup error: name not found: " <> show v
printLookupError (WrongType v) = "lookup error: wrong type: " <> show v
printLookupError (NoValuePresent v) = "lookup error: no value present: " <> show v
