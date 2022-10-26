{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module IOTasks.ValueMap where

import Data.List (sortOn)
import Data.Maybe (mapMaybe, isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (first)

import Data.Typeable (eqT)
import Type.Reflection

import IOTasks.Terms (Var, varname, varExpType, Varname)
import IOTasks.Overflow (I)
import Text.Read (readMaybe)

type ValueMap = Map Var ValueEntry

data ValueEntry = NoEntry | IntegerEntry [(Integer,Int)] | StringEntry [(String,Int)]

-- if there is a unique type for all Varnames in the list and it is the same for all names return return the TypeRep of that type
varnameTypeRep :: [Varname] -> ValueMap -> Maybe SomeTypeRep
varnameTypeRep xs m = uniqueResult $ map (`lookup` Map.keys m) xs

uniqueResult :: Eq a => [Maybe a] -> Maybe a
uniqueResult [] = Nothing
uniqueResult [x] = x
uniqueResult (Nothing:_) = Nothing
uniqueResult (Just x:xs) = (\y -> if x == y then Just y else Nothing) =<< uniqueResult xs

varnameVarList :: [Varname] -> ValueMap -> [Var]
varnameVarList xs = filter ((`elem` xs) . varname) . Map.keys

combinedEntries :: [Var] -> ValueMap -> Maybe ValueEntry
combinedEntries x m
  | isJust (varExpType x) = Just . foldr combineEntries NoEntry $ mapMaybe (`Map.lookup` m) x
  | otherwise = Nothing
  where
    combineEntries NoEntry y = y
    combineEntries x NoEntry = x
    combineEntries (IntegerEntry xs) (IntegerEntry ys) = IntegerEntry $ xs ++ ys
    combineEntries (StringEntry xs) (StringEntry ys) = StringEntry $ xs ++ ys
    combineEntries _ _ = error "combinedEntries: impossible"

-- TODO: before combining each entry is already sorted, switch to mergesort?
sortedEntries :: [Var] -> ValueMap -> Maybe ValueEntry
sortedEntries x m = sortEntry <$> combinedEntries x m

sortEntry :: ValueEntry -> ValueEntry
sortEntry NoEntry = NoEntry
sortEntry (IntegerEntry xs) = IntegerEntry $ sortOn snd xs
sortEntry (StringEntry xs) = StringEntry $ sortOn snd xs

withValueEntry :: ValueEntry -> r -> (forall a. (Typeable a, Show a) => [(a,Int)] -> r) -> r
withValueEntry NoEntry c _ = c
withValueEntry (IntegerEntry xs) _ f = f xs
withValueEntry (StringEntry xs) _ f = f xs

unwrapValueEntry :: forall a. Typeable a => ValueEntry -> [(a,Int)]
unwrapValueEntry NoEntry = []
unwrapValueEntry (IntegerEntry xs) =
  case eqT @a @Integer of
    Just Refl -> xs
    Nothing -> case eqT @a @I of
      Just Refl -> map (first fromInteger) xs
      Nothing -> error $ "unrwarpValue: incompatible type - Integer (or I) and " ++ show (typeRep @a)
unwrapValueEntry (StringEntry xs) =
  case eqT @a @String of
    Just Refl -> xs
    Nothing -> error $ "unrwarpValue: incompatible type - String and " ++ show (typeRep @a)

data Value = IntegerValue Integer | StringValue String

printValue :: Value -> String
printValue (IntegerValue i) = show i
printValue (StringValue s) = s

wrapValue :: forall a. Typeable a => a -> Value
wrapValue =
  case eqT @a @Integer of
    Just Refl -> IntegerValue
    Nothing -> case eqT @a @String of
      Just Refl -> StringValue
      Nothing -> error $ "wrapValue: unsupported type " ++ show (typeRep @a)

withValue :: Value -> (forall a. Typeable a => a -> r) -> r
withValue (IntegerValue i) f = f i
withValue (StringValue s) f = f s

unwrapValue :: forall a. Typeable a => Value -> a
unwrapValue v = withValue v $ \(x :: b) ->
  case eqT @a @b of
    Just Refl -> x
    Nothing -> error $ "unrwarpValue: incompatible type - "++ show (typeRep @b) ++ " and " ++ show (typeRep @a)

readValue :: forall a. (Typeable a, Read a) => String -> a
readValue x =
  case eqT @a @String of
    Just Refl -> x
    Nothing -> case readMaybe @a x of
      Just x -> x
      Nothing -> error $ x ++ " - " ++ show (typeRep @a)

showValue :: forall a. (Typeable a, Show a) => a -> String
showValue x = case eqT @a @String of
  Just Refl -> x
  Nothing -> show x

insertValue :: (Value,Int) -> Var -> ValueMap -> ValueMap
insertValue (v,i) k = Map.alter (f v) k
  where
    f (IntegerValue x) Nothing = Just $ IntegerEntry [(x,i)]
    f (IntegerValue x) (Just NoEntry) = Just $ IntegerEntry [(x,i)]
    f (IntegerValue x) (Just (IntegerEntry xs)) = Just $ IntegerEntry $ (x,i):xs
    f (StringValue x) Nothing = Just $ StringEntry [(x,i)]
    f (StringValue x) (Just NoEntry) = Just $ StringEntry [(x,i)]
    f (StringValue x) (Just (StringEntry xs)) = Just $ StringEntry $ (x,i):xs
    f _ _ = error $ "insertValue: type mismatch for variable " ++ varname k

lookupInteger :: Var -> ValueMap -> Maybe [(Integer,Int)]
lookupInteger v m = do
  r <- Map.lookup v m
  case r of
    NoEntry -> Just []
    IntegerEntry xs -> Just xs
    StringEntry _ -> Nothing

lookupString :: Var -> ValueMap -> Maybe [(String,Int)]
lookupString v m = do
  r <- Map.lookup v m
  case r of
    NoEntry -> Just []
    IntegerEntry _ -> Nothing
    StringEntry xs -> Just xs
