{-# LANGUAGE PartialTypeSignatures #-}
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

inputGenerator' :: CoreSpec -> Gen [(Value,Int)]
inputGenerator' spec =
  let act = concat . catMaybes <$> mapM atomicGenerator spec
  in evalStateT act []

atomicGenerator :: AtomicSpec -> GenEnvM (Maybe [(Value,Int)])
atomicGenerator (In (n,Base NumTy)) = do
  i <- lift intGen
  let v = IntVal i
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (n,Base StringTy)) = do
  xs <- lift lineGen
  let v = Line xs
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (n,DPred NumTy f name)) = do
  (Just (IntVal iOrig)) <- gets $ lookup name
  let i = f iOrig
      v = IntVal i
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (n,DPred StringTy f name)) = do
  (Just (Line iOrig)) <- gets $ lookup name
  let xs = f iOrig
      v = Line xs
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (n,SPred NumTy p)) = do
  i <- lift $ intGen `suchThat` p
  let v = IntVal i
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (n,SPred StringTy p)) = do
  xs <- lift $ lineGen `suchThat` p
  let v = Line xs
  modify ((n,v):)
  return $ Just [(v,1)]
atomicGenerator (In (xs,SListTy ty n)) = do
  (Just (IntVal i)) <- gets $ lookup n
  let bTy = baseType ty
  list <- listGen bTy ty i
  modify ((xs,listConstr bTy list):)
  return . Just $ zipWith (\x y -> (valueConstr bTy x, y)) list [i,i-1..0]
atomicGenerator (In (xs,DListTy ty cond)) = do
  env <- get
  i <- lift $ choose (length cond,10)
  let bTy = baseType ty
  list <- listGen' bTy (\vs -> hasType env (listConstr bTy vs) (DListTy ty cond)) ty i
  modify ((xs,listConstr bTy list):)
  return . Just $  zipWith (\x y -> (valueConstr bTy x, y)) list [i,i-1..0]
atomicGenerator (Out _) = return Nothing

intGen :: Gen Int
intGen = choose (-10,10)

lineGen :: Gen String
lineGen = do
  len <- choose (0,10)
  vectorOf len $ oneof [choose ('a','z'), choose ('A','Z')]

listGen :: BaseType q -> InputType 'NonList q -> Int -> GenEnvM [q]
listGen ty = listGen' ty (const True)

listGen' :: BaseType q -> ([q] -> Bool) -> InputType 'NonList q -> Int -> GenEnvM [q]
listGen' NumTy p (Base NumTy) n = lift $ vectorOf n intGen `suchThat` p
listGen' NumTy p (SPred NumTy q) n = lift $ vectorOf n (intGen `suchThat` q) `suchThat` p
listGen' NumTy p (DPred NumTy f vname) n = do
  (Just (IntVal z)) <- gets $ lookup vname
  lift $ vectorOf n (elements [f z]) `suchThat` p
listGen' StringTy p (Base StringTy) n = lift $ vectorOf n lineGen `suchThat` p
listGen' StringTy p (SPred StringTy q) n = lift $ vectorOf n (lineGen `suchThat` q) `suchThat` p
listGen' StringTy p (DPred StringTy f vname) n = do
  (Just (Line xs)) <- gets $ lookup vname
  lift $ vectorOf n (elements [f xs]) `suchThat` p
