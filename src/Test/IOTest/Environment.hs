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
module Test.IOTest.Environment
  (Environment
  , Test.IOTest.Environment.update
  , updateWithValue
  , freshEnvironment
  , lookupNameAtType
  , HasVariables(..)
  , Varname
  , printLookupError
  ) where

import Test.IOTest.Utils
import Test.IOTest.Value

import Data.Kind
import Data.Dynamic
import Data.Proxy
import Type.Reflection

import Data.Maybe
import Control.Applicative
import           Control.Monad                  ( (>=>) )
import Data.List.NonEmpty as NonEmpty
import Data.HashMap.Strict as Map

type Varname = String

data Entry where
  EmptyEntry :: Entry
  MkEntry :: StringEmbedding a => TypeRep a -> NonEmpty a -> Entry

instance Show Entry where
  show EmptyEntry = "EmptyEntry"
  show (MkEntry r vs) = "(" <> show r <> ", " <> show (pack <$> vs) <>")"

newEntry :: forall (a :: Type). (Typeable a, StringEmbedding a) => a -> Entry
newEntry x = MkEntry (typeOf x) (x :| [])

addValue :: (Typeable a, StringEmbedding a) => a -> Entry -> Maybe Entry
addValue x EmptyEntry = Just $ MkEntry typeRep (x :| [])
addValue x (MkEntry r (hd :| tl)) =
  case typeOf x `eqTypeRep` r of
    Just HRefl -> Just $ MkEntry r (hd :| tl ++ [x])
    Nothing -> Nothing

newtype Environment = MkEnvironment (HashMap Varname Entry)

newEnvironment :: Environment
newEnvironment = MkEnvironment Map.empty

freshEnvironment :: HasVariables a => a -> Environment
freshEnvironment s =
  let addNames = Prelude.foldr ((>=>) . addName) return $ vars s
  in fromJust $ addNames newEnvironment

addName :: Varname -> Environment -> Maybe Environment
addName x (MkEnvironment xs) =
  case Map.lookup x xs of
    Just _ -> Nothing
    Nothing -> Just $ MkEnvironment (Map.insert x EmptyEntry xs)

update :: (Typeable a, StringEmbedding a) => Varname -> a -> Environment -> Maybe Environment
update x v = updateWithValue x (Value typeRep v)

updateWithValue :: Varname -> Value -> Environment -> Maybe Environment
updateWithValue x (Value vRep v) (MkEnvironment es) =
  case Map.lookup x es of
    Nothing -> Nothing
    Just EmptyEntry -> Just $ MkEnvironment $ Map.update (\EmptyEntry -> Just $ newEntry v) x es
    Just (MkEntry eRep _) ->
      case vRep `eqTypeRep` eRep of
        Nothing -> Nothing
        Just HRefl -> Just $ MkEnvironment $ Map.update (\e -> addValue v e <|> Just e) x es

lookupNameAtType :: Typeable a => Proxy a -> Varname -> Environment -> Either LookupError [a]
lookupNameAtType (_ :: Proxy a) x (MkEnvironment es) =
  case Map.lookup x es of
    Nothing -> Left $ NameNotFound $ x <> " in " <> show es
    Just EmptyEntry -> Right []
    Just (MkEntry r vs) ->
      case typeRep @a `eqTypeRep` r of
        Just HRefl -> Right $ NonEmpty.toList vs
        Nothing -> Left $ WrongType $ x  <> " in " <> show es

data LookupError = NameNotFound String | WrongType String deriving Show

printLookupError :: LookupError -> String
printLookupError (NameNotFound e) = "lookup error: name not found: " <> e
printLookupError (WrongType e) = "lookup error: wrong type: " <> e

class HasVariables a where
  vars :: a -> [Varname]