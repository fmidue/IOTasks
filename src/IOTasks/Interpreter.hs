{-# LANGUAGE LambdaCase #-}
module IOTasks.Interpreter where

import Prelude hiding (readLn,putStrLn)

import Control.Monad.State
import Control.Monad.Loops (iterateUntil)

import qualified Data.Map as Map
import qualified Data.Set as Set

import IOTasks.Specification
import IOTasks.MonadTeletype
import IOTasks.Term
import IOTasks.ValueSet
import IOTasks.Trace
import IOTasks.OutputPattern

interpret :: MonadTeletype m => Specification -> [m ()]
interpret s = do
  collapsed <- collapseChoice s
  pure $ flip evalStateT Map.empty $
    sem
      (\n x vs m -> RecSub (x,vs,m,n) (n+1))
      (\case
        RecSub (x,_,AssumeValid,n) p' -> do
          v <- lift readLn
          modify (Map.alter (\case {Just xs -> Just $ (v,n):xs; Nothing -> Just [(v,n)]}) x)
          p'
        RecSub (x,vs,Abort,n) p' -> do
          v <- lift readLn
          when (vs `containsValue` v) $ do
            modify (Map.alter (\case {Just xs -> Just $ (v,n):xs; Nothing -> Just [(v,n)]}) x)
            p'
        RecSub (x,vs,UntilValid,n) p' -> do
          v <- iterateUntil (vs `containsValue`) $ lift readLn
          modify (Map.alter (\case {Just xs -> Just $ (v,n):xs; Nothing -> Just [(v,n)]}) x)
          p'
        NoRec{} -> error "interpret: impossible"
        RecSame{} -> error "interpret: impossible"
        RecBoth{} -> error "interpret: impossible")
      (\_ Mandatory ts p' -> do
        if Set.size ts == 1
          then do
            e <- get
            lift $ putStrLn $ printPattern $ snd $ evalPattern e (Set.elemAt 0 ts)
            p'
          else error "interpret: impossible"
      )
      (\_ cond pl pr -> do
        e <- get
        if snd $ eval cond e
          then pl
          else pr
      )
      (pure ())
      1
      collapsed

collapseChoice :: Specification -> [Specification]
collapseChoice (ReadInput x vs m s') = ReadInput x vs m <$> collapseChoice s'
collapseChoice (WriteOutput o ts s') = do
  WriteOutput Mandatory <$> (Set.singleton <$> Set.toList ts) <*> collapseChoice s' ++ if o == Optional then collapseChoice s' else []
collapseChoice (Branch p l r s') = Branch p <$> collapseChoice l <*> collapseChoice r <*> collapseChoice s'
collapseChoice (TillE bdy s') = TillE <$> collapseChoice bdy <*> collapseChoice s'
collapseChoice Nop = pure Nop
collapseChoice E = pure E
