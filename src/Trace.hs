{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Trace where

import Interaction()
import Types
import IOtt

import Control.Monad.State

import Data.List
import Data.Maybe

data ProtoTrace
  = Step OptTy Action ProtoTrace
  | End
  deriving Show

data Action
  = Input
  | Output Matcher
  | OneOf [Action]
  deriving Show

buildPTrace :: [(AtomicSpec, Interaction)] -> [(String,Int)] -> ProtoTrace
buildPTrace spec ys = evalState (buildPTrace' spec ys) []

buildPTrace' :: [(AtomicSpec, Interaction)] -> [(String,Int)] -> State Env ProtoTrace
buildPTrace' [] _ = return End
buildPTrace' (x:xs) ys =
  case x of
    -- single values
    (In (name, IntTy), OnInput optB optF optA) -> noListHelper name optB optF optA
    (In (name, NatTy), OnInput optB optF optA) -> noListHelper name optB optF optA
    (In (name, Neg _), OnInput optB optF optA) -> noListHelper name optB optF optA
    -- list values
    (In (name, SListTy _ n), OnInput optB optF optA) -> do
      (Just (IntVal i)) <- gets $ lookup n
      listHelper name optB optF optA i
    (In (name, DListTy _ _), OnInput optB optF optA) -> do
      let i = snd $ head ys
      listHelper name optB optF optA i
    -- output
    (Out ((SZ,v),[]), OnOutput d) -> do
      let action = applyOpt d
      Step Must action <$> buildPTrace' xs ys
    (Out ((Unary,f),[name]), OnOutput d) -> do
      (Just v) <- gets $ lookup name
      let output = f v
          action = applyOpt d
      Step Must action <$> buildPTrace' xs ys
    (Out ((Binary,f),[name1,name2]), OnOutput d) -> do
      (Just v1) <- gets $ lookup name1
      (Just v2) <- gets $ lookup name2
      let output = f v1 v2
          action = applyOpt d
      Step Must action <$> buildPTrace' xs ys
    -- fallback
    _ -> error $ "ill-formed spec!\n" ++ show x
    where
      noListHelper name optB optF optA = do
          let i = read . fst $ head ys
              react = case optF of
                Just (Must, f) -> Step Must (Output . f $ IntVal i)
                Just (May, f) -> Step May (Output . f $ IntVal i)
                Nothing -> id
          modify ((name,IntVal i):)
          optOutput optB . Step Must Input . react . optOutput optA <$> buildPTrace' xs (tail ys)

      listHelper name optB optF optA i = do
        let list = read . fst <$> take i ys
            react k = case optF of
              Just (Must, f) -> Step Must (Output . f . IntVal $ list !! k)
              Just (May, f) -> Step May (Output . f $ IntVal $ list !! k)
              Nothing -> id
            step k = optOutput optB . Step Must Input . react k . optOutput optA
            ins = foldr (\k g -> step k . g) id [0..i-1]
        modify ((name,ListVal list):)
        ins <$> buildPTrace' xs (drop i ys)

optOutput :: Opt Matcher -> (ProtoTrace -> ProtoTrace)
optOutput (Just (Must, xs)) = Step Must (Output xs)
optOutput (Just (May, xs)) = Step May (Output xs)
optOutput Nothing = id

applyOpt :: Opt Matcher -> Action
applyOpt (Just (Must, m)) = Output m
applyOpt (Just (May, m)) = OneOf [Output matchValue, Output m]
applyOpt Nothing = Output matchValue

checkAgainstProto :: Output () -> ProtoTrace -> (Bool, String)
checkAgainstProto o t =
  let r = checkAgainstProto' o t
      b = any fst r
      err = fromMaybe "" $ find (not.null) $ snd <$> r
  in (b,err)

-- produced error messages are not optimal, since they regard optional specs in the same way as mandatory ones.
-- This can lead to confusing feedback. Basically the msg just states why False was produced at that point in the computation.
-- So the context is ignored
checkAgainstProto' :: Output () -> ProtoTrace -> [(Bool,String)]
-- Read cases
checkAgainstProto' (Read x) (Step Must Input y) = checkAgainstProto' x y
checkAgainstProto' (Read x) (Step May Input y) = checkAgainstProto' x y ++ checkAgainstProto' (Read x) y
checkAgainstProto' (Read x) (Step May (Output _) y) = checkAgainstProto' (Read x) y
checkAgainstProto' (Read _) (Step Must (Output s) _) = [(False,"Expected output of form '"++ outputForm s ++"', but program reads input")]
checkAgainstProto' (Read x) (Step Must (OneOf as) y) = concatMap (checkAgainstProto' (Read x) . (\a -> Step Must a y)) as
checkAgainstProto' (Read x) (Step May (OneOf as) y) = concatMap (checkAgainstProto' (Read x) . (\a -> Step May a y)) as
checkAgainstProto' (Read _) End = [(False,"Expected termination, but program reads input")]
-- Write cases
checkAgainstProto' (Write s _) (Step Must Input _) = [(False,"Expected input, but program outputs '"++ s ++"'")]
checkAgainstProto' (Write s x) (Step May Input y) = checkAgainstProto' (Write s x) y
checkAgainstProto' (Write s x) (Step Must (Output s') y) = checkOutput s s' <$> checkAgainstProto' x y
checkAgainstProto' (Write s x) (Step May (Output s') y) = (checkOutput s s' <$> checkAgainstProto' x y) ++ checkAgainstProto' (Write s x) y
checkAgainstProto' (Write s x) (Step Must (OneOf as) y) = concatMap (checkAgainstProto' (Write s x) . (\a -> Step Must a y)) as
checkAgainstProto' (Write s x) (Step May (OneOf as) y) = concatMap (checkAgainstProto' (Write s x) . (\a -> Step May a y)) as
checkAgainstProto' (Write s _) End = [(False,"Expected termination, but program outputs '" ++ s ++ "'")]
-- Finish cases
checkAgainstProto' (Finish _) End = [(True,"")]
checkAgainstProto' (Finish _) _ = [(False,"Expected more steps, but program ended")]
-- OutOfInputs cases
checkAgainstProto' OutOfInputs _ = [(False,"To much reads! No more inputs left")]

checkOutput :: String -> Matcher -> (Bool, String) -> (Bool,String)
checkOutput s s' (b,xs) =
  if match s' s
  then (b,xs)
  else (False,"Expected output of form '" ++ outputForm s' ++ "', but got '" ++ s ++ "'.")
