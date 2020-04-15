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
module Data.Environment (
  VarEnv(..),
  store,
  PVarEnv(..),
  lookupLastAtType,
  Environment,
  Value,
  value,
  Varname,
  LookupError(..),
  printLookupError,
  ) where

import Data.Environment.Class
import Data.Environment.Value

import Data.Dynamic
import Data.Proxy
import Type.Reflection

import Data.Maybe
import Control.Applicative
import Control.Monad ((>=>))

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty (toList)

import Data.Map (Map)
import qualified Data.Map as Map (lookup, update, empty, insert)

type Varname = String

data Entry where
  EmptyEntry :: Entry
  MkEntry :: TypeRep a -> Value -> NonEmpty Value -> Entry

instance Show Entry where
  show EmptyEntry = "EmptyEntry"
  show (MkEntry r v vs) = "(" <> show r <> ", " <> show v <> ", " <> show vs <>")"

addValue :: Value -> Entry -> Maybe Entry
addValue v@(Value tyRep _ _) EmptyEntry = Just $ MkEntry tyRep v (v :| [])
addValue v@(Value tyRep _ _) (MkEntry r _ (hd :| tl)) =
  case tyRep `eqTypeRep` r of
    Just HRefl -> Just $ MkEntry r v (hd :| tl ++ [v])
    Nothing -> Nothing

newtype Environment v = MkEnvironment (Map v Entry) deriving Show

instance Ord v => VarEnv Environment v where
  -- storeValue :: Varname -> Value -> (Environment v) -> Maybe (Environment v)
  storeValue x v@(Value vRep _ _) (MkEnvironment es) =
    case Map.lookup x es of
      Nothing -> Nothing
      Just EmptyEntry -> updateEntry
      Just (MkEntry eRep _ _) ->
        case vRep `eqTypeRep` eRep of
          Nothing -> Nothing
          Just HRefl -> updateEntry
    where updateEntry = Just $ MkEnvironment $ Map.update (\e -> addValue v e <|> Just e) x es

  lookupAtType p x e = lookupNameAtType p x e >>= \case
    (Nothing,_) -> Left $ NoValuePresent x
    (Just v,_) -> Right v

  freshEnvironment vs =
    let addNames = Prelude.foldr ((>=>) . addName) return vs
    in fromJust $ addNames emptyEnvironment

  emptyEnvironment = MkEnvironment Map.empty

instance Ord v => PVarEnv Environment v where
  lookupAllAtType p v e = snd <$> lookupNameAtType p v e

addName :: Ord v => v -> Environment v -> Maybe (Environment v)
addName x (MkEnvironment xs) =
  case Map.lookup x xs of
    Just _ -> Nothing
    Nothing -> Just $ MkEnvironment (Map.insert x EmptyEntry xs)

lookupNameAtType :: (Typeable a, Ord v) => Proxy a -> v -> Environment v -> Either (LookupError v) (Maybe a, [a])
lookupNameAtType (_ :: Proxy a) x (MkEnvironment es) =
  case Map.lookup x es of
    Nothing -> Left $ NameNotFound x
    Just EmptyEntry -> Right (Nothing,[])
    Just (MkEntry r v vs) ->
      case typeRep @a `eqTypeRep` r of
        Just HRefl -> Right (Just (unsafeExtract v), map unsafeExtract (NonEmpty.toList vs))
        Nothing -> Left $ WrongType x
