{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Core where

import Prelude hiding (getLine, putStrLn, print)

import Types

import Data.Maybe

import Control.Monad.State.Lazy
import Control.Arrow

import Test.QuickCheck

-- test case generation
type GenEnvM = StateT Env Gen

inputGenerator :: CoreSpec -> Gen [(String,Int)]
inputGenerator = ((first show <$>) <$>) . inputGenerator'

inputGenerator' :: CoreSpec -> Gen [(Int,Int)]
inputGenerator' spec =
  let act = concat . catMaybes <$> mapM atomicGenerator spec
  in evalStateT act []

atomicGenerator :: AtomicSpec -> GenEnvM (Maybe [(Int,Int)])
atomicGenerator (In (n,IntTy)) = do
  i <- lift intGen
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (n,Neg name)) = do
  (Just (IntVal iOrig)) <- gets $ lookup name
  let i = negate iOrig
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (n,Exact x)) = do
  modify ((n,IntVal x):)
  return $ Just [(x,1)]
atomicGenerator (In (n,NatTy)) = do
  i <- lift natGen
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (xs,SListTy ty n)) = do
  (Just (IntVal i)) <- gets $ lookup n
  list <- listGen ty i
  modify ((xs,ListVal list):)
  return . Just $ zip list [i,i-1..0]
atomicGenerator (In (xs,DListTy ty cond)) = do
  env <- get
  i <- lift $ choose (length cond,10)
  list <- listGen' (\vs -> hasType env (ListVal vs) (DListTy ty cond)) ty i
  modify ((xs,ListVal list):)
  return . Just $ zip list [i,i-1..0]
atomicGenerator (Out _) = return Nothing

intGen :: Gen Int
intGen = choose (-10,10)

natGen :: Gen Int
natGen = choose (0,10)

listGen :: InputType 'NonList -> Int -> GenEnvM [Int]
listGen = listGen' (const True)

listGen' :: ([Int] -> Bool) -> InputType 'NonList -> Int -> GenEnvM [Int]
listGen' p IntTy n = lift $ vectorOf n intGen `suchThat` p
listGen' p NatTy n = lift $ vectorOf n natGen `suchThat` p
listGen' p (Exact x) n = lift $ vectorOf n (elements [x]) `suchThat` p
listGen' p (Neg i) n = do
  (Just (IntVal z)) <- gets $ lookup i
  lift $ vectorOf n (elements [z]) `suchThat` p
