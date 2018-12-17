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
atomicGenerator (In (n,Base NumTy)) = do
  i <- lift intGen
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (n,DPred NumTy f name)) = do
  (Just (IntVal iOrig)) <- gets $ lookup name
  let i = f iOrig
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (n,SPred NumTy p)) = do
  i <- lift $ intGen `suchThat` p
  modify ((n,IntVal i):)
  return $ Just [(i,1)]
atomicGenerator (In (xs,SListTy ty n)) = do
  (Just (IntVal i)) <- gets $ lookup n
  list <- listGen ty i
  modify ((xs,IListVal list):)
  return . Just $ zip list [i,i-1..0]
atomicGenerator (In (xs,DListTy ty cond)) = do
  env <- get
  i <- lift $ choose (length cond,10)
  list <- listGen' (\vs -> hasType env (IListVal vs) (DListTy ty cond)) ty i
  modify ((xs,IListVal list):)
  return . Just $ zip list [i,i-1..0]
atomicGenerator (Out _) = return Nothing

intGen :: Gen Int
intGen = choose (-10,10)

natGen :: Gen Int
natGen = choose (0,10)

listGen :: InputType a -> Int -> GenEnvM [Int]
listGen = listGen' (const True)

listGen' :: ([Int] -> Bool) -> InputType a -> Int -> GenEnvM [Int]
listGen' p (Base NumTy) n = lift $ vectorOf n intGen `suchThat` p
listGen' p (SPred NumTy q) n = lift $ vectorOf n (intGen `suchThat` q) `suchThat` p
listGen' p (DPred NumTy f vname) n = do
  (Just (IntVal z)) <- gets $ lookup vname
  lift $ vectorOf n (elements [f z]) `suchThat` p
