{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.Environment
  (Environment
  , store
  , storeValue
  , Value(..)
  , freshEnvironment
  , lookupAllAtType
  , lookupLastAtType
  , Varname
  , LookupError
  , printLookupError
  ) where

import Test.IOTasks.Utils
import Test.IOTasks.Value

import Data.Dynamic
import Data.Proxy
import Type.Reflection

import Data.Maybe
import Control.Applicative
import           Control.Monad                  ( (>=>) )
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map

type Varname = String

data Entry where
  EmptyEntry :: Entry
  MkEntry :: StringEmbedding a => TypeRep a -> a -> NonEmpty a -> Entry

instance Show Entry where
  show EmptyEntry = "EmptyEntry"
  show (MkEntry r v vs) = "(" <> show r <> ", " <> pack v <> ", " <> show (pack <$> vs) <>")"

newEntry :: (Typeable a, StringEmbedding a) => a -> Entry
newEntry x = MkEntry typeRep x (x :| [])

addValue :: (Typeable a, StringEmbedding a) => a -> Entry -> Maybe Entry
addValue x EmptyEntry = Just $ newEntry x
addValue x (MkEntry r _ (hd :| tl)) =
  case typeOf x `eqTypeRep` r of
    Just HRefl -> Just $ MkEntry r x (hd :| tl ++ [x])
    Nothing -> Nothing

newtype Environment = MkEnvironment (Map Varname Entry) deriving Show

newEnvironment :: Environment
newEnvironment = MkEnvironment Map.empty

freshEnvironment :: [Varname] -> Environment
freshEnvironment vs =
  let addNames = Prelude.foldr ((>=>) . addName) return vs
  in fromJust $ addNames newEnvironment

addName :: Varname -> Environment -> Maybe Environment
addName x (MkEnvironment xs) =
  case Map.lookup x xs of
    Just _ -> Nothing
    Nothing -> Just $ MkEnvironment (Map.insert x EmptyEntry xs)

store :: (Typeable a, StringEmbedding a) => Varname -> a -> Environment -> Maybe Environment
store x = storeValue x . Value typeRep

storeValue :: Varname -> Value -> Environment -> Maybe Environment
storeValue x (Value vRep v) (MkEnvironment es) =
  case Map.lookup x es of
    Nothing -> Nothing
    Just EmptyEntry -> updateEntry
    Just (MkEntry eRep _ _) ->
      case vRep `eqTypeRep` eRep of
        Nothing -> Nothing
        Just HRefl -> updateEntry
  where updateEntry = Just $ MkEnvironment $ Map.update (\e -> addValue v e <|> Just e) x es

lookupNameAtType :: Typeable a => Proxy a -> Varname -> Environment -> Either LookupError (Maybe a, [a])
lookupNameAtType (_ :: Proxy a) x (MkEnvironment es) =
  case Map.lookup x es of
    Nothing -> Left $ NameNotFound $ x <> " in " <> show es
    Just EmptyEntry -> Right (Nothing,[])
    Just (MkEntry r v vs) ->
      case typeRep @a `eqTypeRep` r of
        Just HRefl -> Right (Just v, NonEmpty.toList vs)
        Nothing -> Left $ WrongType $ x  <> " in " <> show es

lookupAllAtType :: Typeable a => Proxy a -> Varname -> Environment -> Either LookupError [a]
lookupAllAtType p v e = snd <$> lookupNameAtType p v e

lookupLastAtType :: Typeable a => Proxy a -> Varname -> Environment -> Either LookupError a
lookupLastAtType p x e@(MkEnvironment es) = lookupNameAtType p x e >>= \case
  (Nothing,_) -> Left $ NoValuePresent $ x  <> " in " <> show es
  (Just v,_) -> Right v

data LookupError = NameNotFound String | WrongType String | NoValuePresent String deriving Show

printLookupError :: LookupError -> String
printLookupError (NameNotFound e) = "lookup error: name not found: " <> e
printLookupError (WrongType e) = "lookup error: wrong type: " <> e
printLookupError (NoValuePresent e) = "lookup error: no value present: " <> e
