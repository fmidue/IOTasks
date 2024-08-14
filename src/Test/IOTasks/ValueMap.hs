{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Test.IOTasks.ValueMap (
  ValueMap,
  emptyValueMap, insertValue,
  Test.IOTasks.ValueMap.lookup,
  sortedEntries,
  varnameTypeRep, varnameVarList,
  Value(..),
  wrapValue, unwrapValue,
  readValue, showValue,
  ValueEntry(..),
  withValueEntry,
  unwrapValueEntry,
  lookupInteger, lookupString,
  ) where

import Data.List as List (sortOn, lookup)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Type.Reflection

import Test.IOTasks.Var (SomeVar(..), someVarname, Varname, Embedded (Embedded), Var (..), someVar, varTypeRep, SomeConsistentVars, someConsistentVars)
import Text.Read (readMaybe)
import Type.Match (fallbackCase', inCaseOf, inCaseOfApp, matchTypeOf)
import Data.Bifunctor (first)

data ValueMap = ValueMap { valueMap :: Map SomeVar ValueEntry, size :: Int } deriving (Eq,Show)

data ValueEntry = NoEntry | IntegerEntry [(Integer,Int)] | StringEntry [(String,Int)] deriving (Eq,Show)

emptyValueMap :: [SomeVar] -> ValueMap
emptyValueMap xs = ValueMap (Map.fromList ((,NoEntry) <$> xs)) 0

lookup :: SomeVar -> ValueMap -> Maybe ValueEntry
lookup k = Map.lookup k . valueMap

-- if there is a unique type for all Varnames in the list and it is the same for all names return the TypeRep of that type
varnameTypeRep :: [Varname] -> ValueMap -> Maybe SomeTypeRep
varnameTypeRep xs m = uniqueResult $ map (`List.lookup` map varInfo (Map.keys $ valueMap m)) xs
  where
    varInfo :: SomeVar -> (Varname, SomeTypeRep)
    varInfo (SomeVar (IntVar x)) = (x,SomeTypeRep $ typeRep @Integer)
    varInfo (SomeVar (StringVar x)) = (x, SomeTypeRep $ typeRep @String)
    varInfo (SomeVar (EmbeddedVar ty x)) = (x, SomeTypeRep $ App (typeRep @Embedded) ty)

uniqueResult :: Eq a => [Maybe a] -> Maybe a
uniqueResult [] = Nothing
uniqueResult [x] = x
uniqueResult (Nothing:_) = Nothing
uniqueResult (Just x:xs) = (\y -> if x == y then Just y else Nothing) =<< uniqueResult xs

varnameVarList :: [Varname] -> ValueMap -> Maybe SomeConsistentVars
varnameVarList xs = someConsistentVars . filter ((`elem` xs) . someVarname) . Map.keys . valueMap

combinedEntries :: Typeable a => [Var a] -> ValueMap -> ValueEntry
combinedEntries x m = foldr combineEntries NoEntry $ mapMaybe ((`Map.lookup` valueMap m) . someVar) x
  where
    combineEntries NoEntry y = y
    combineEntries x NoEntry = x
    combineEntries (IntegerEntry xs) (IntegerEntry ys) = IntegerEntry $ xs ++ ys
    combineEntries (StringEntry xs) (StringEntry ys) = StringEntry $ xs ++ ys
    combineEntries _ _ = error "combinedEntries: impossible"

-- TODO: before combining each entry is already sorted, switch to merge sort?
sortedEntries :: Typeable a => [Var a] -> ValueMap -> ValueEntry
sortedEntries x m = sortEntry $ combinedEntries x m

sortEntry :: ValueEntry -> ValueEntry
sortEntry NoEntry = NoEntry
sortEntry (IntegerEntry xs) = IntegerEntry $ sortOn snd xs
sortEntry (StringEntry xs) = StringEntry $ sortOn snd xs

withValueEntry :: ValueEntry -> r -> (forall a. (Typeable a, Show a) => [(a,Int)] -> r) -> r
withValueEntry NoEntry c _ = c
withValueEntry (IntegerEntry xs) _ f = f xs
withValueEntry (StringEntry xs) _ f = f xs

unwrapValueEntry :: Var a -> ValueEntry -> [(a,Int)]
unwrapValueEntry _ NoEntry = []

unwrapValueEntry IntVar{} (IntegerEntry xs) = xs
unwrapValueEntry StringVar{} (IntegerEntry _) = error "unwrapValue: incompatible type - Integer (or I) and String"
unwrapValueEntry EmbeddedVar{} (IntegerEntry xs) = map (first Embedded) xs

unwrapValueEntry StringVar{} (StringEntry xs) = xs
unwrapValueEntry IntVar{} (StringEntry _) = error "unwrapValue: incompatible type - String and Integer (or I)"
unwrapValueEntry (EmbeddedVar ty _) (StringEntry _) = error $ "unwrapValue: incompatible type - String and " ++ show (App (typeRep @Embedded) ty)

data Value = IntegerValue Integer | StringValue String deriving Show

showValue :: Value -> String
showValue (IntegerValue i) = show i
showValue (StringValue s) = s

wrapValue :: Typeable a => a -> Value
wrapValue = wrapValue' where
  wrapValue' :: forall a. Typeable a => a -> Value
  wrapValue' x =
    matchTypeOf x
      [ inCaseOf @Integer IntegerValue
      , inCaseOf @String StringValue
      , inCaseOfApp @Embedded $ \HRefl (Embedded i) -> IntegerValue i
      , fallbackCase' $ error $ "wrapValue: unsupported type " ++ show (typeRep @a)
      ]

unwrapValue :: Var a -> Value -> a
unwrapValue IntVar{} (IntegerValue i) = i
unwrapValue StringVar{} (StringValue x) = x
unwrapValue EmbeddedVar{} (IntegerValue i) = Embedded i
unwrapValue x IntegerValue{} = error $ "unwrapValue: incompatible type - Integer and " ++ show (varTypeRep x)
unwrapValue x StringValue{} = error $ "unwrapValue: incompatible type - String and " ++ show (varTypeRep x)

readValue :: Var a -> String -> a
readValue = readValue' where
  readValue' :: Var a -> String -> a
  readValue' IntVar{} x = case readMaybe x of
    Just i -> i
    Nothing -> error $ x ++ " - Integer"
  readValue' StringVar{} x = x
  readValue' (EmbeddedVar (ty :: TypeRep a) _) x =
    case readMaybe @(Embedded a) x of
      Just v -> v
      Nothing -> error $ x ++ " - Embedded " ++ show ty

insertValue :: Value -> SomeVar -> ValueMap -> ValueMap
insertValue v k (ValueMap m sz)
  | v `hasType` k = ValueMap (Map.alter (f v) k m) i
  | otherwise = error $ "insertValue: type mismatch for variable " ++ someVarname k
  where
    i = sz + 1
    f (IntegerValue x) Nothing = Just $ IntegerEntry [(x,i)]
    f (IntegerValue x) (Just NoEntry) = Just $ IntegerEntry [(x,i)]
    f (IntegerValue x) (Just (IntegerEntry xs)) = Just $ IntegerEntry $ (x,i):xs
    f (StringValue x) Nothing = Just $ StringEntry [(x,i)]
    f (StringValue x) (Just NoEntry) = Just $ StringEntry [(x,i)]
    f (StringValue x) (Just (StringEntry xs)) = Just $ StringEntry $ (x,i):xs
    f _ _ = error $ "insertValue: type mismatch for variable " ++ someVarname k

hasType :: Value -> SomeVar -> Bool
hasType (IntegerValue _) (SomeVar IntVar{}) = True
hasType (IntegerValue _) (SomeVar EmbeddedVar{}) = True
hasType (StringValue _) (SomeVar StringVar{}) = True
hasType _ _ = False

lookupInteger :: SomeVar -> ValueMap -> Maybe [(Integer,Int)]
lookupInteger v m = do
  r <- Map.lookup v $ valueMap m
  case r of
    NoEntry -> Just []
    IntegerEntry xs -> Just xs
    StringEntry _ -> Nothing

lookupString :: SomeVar -> ValueMap -> Maybe [(String,Int)]
lookupString v m = do
  r <- Map.lookup v $ valueMap m
  case r of
    NoEntry -> Just []
    IntegerEntry _ -> Nothing
    StringEntry xs -> Just xs
