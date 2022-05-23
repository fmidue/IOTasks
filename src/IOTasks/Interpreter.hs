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
      (\() x vs m -> RecSub (x,vs,m) ())
      (\case
        RecSub (x,_,AssumeValid) p' -> do
          v <- lift readLn
          modify (Map.alter (\case {Just xs -> Just $ v:xs; Nothing -> Just [v]}) x)
          p'
        RecSub (x,vs,UntilValid) p' -> do
          v <- iterateUntil (vs `containsValue`) $ lift readLn
          modify (Map.alter (\case {Just xs -> Just $ v:xs; Nothing -> Just [v]}) x)
          p'
        _ -> error "interpret: impossible")
      (\() Mandatory ts p' -> do
        if Set.size ts == 1
          then do
            e <- get
            lift $ putStrLn $ printPattern $ evalPattern e (Set.elemAt 0 ts)
            p'
          else error "interpret: impossible"
      )
      (\() cond pl pr -> do
        e <- get
        if eval cond e
          then pl
          else pr
      )
      (pure ())
      ()
      collapsed

collapseChoice :: Specification -> [Specification]
collapseChoice (ReadInput x vs m s') = ReadInput x vs m <$> collapseChoice s'
collapseChoice (WriteOutput o ts s') = do
  WriteOutput Mandatory <$> (Set.singleton <$> Set.toList ts) <*> collapseChoice s' ++ if o == Optional then collapseChoice s' else []
collapseChoice (Branch p l r s') = Branch p <$> collapseChoice l <*> collapseChoice r <*> collapseChoice s'
collapseChoice (TillE bdy s') = TillE <$> collapseChoice bdy <*> collapseChoice s'
collapseChoice Nop = pure Nop
collapseChoice E = pure E
