{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.Interpreter where

import Prelude hiding (readLn,putStrLn)

import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set

import IOTasks.Specification
import IOTasks.MonadTeletype as MTT
import IOTasks.Term
import IOTasks.ValueSet
import IOTasks.Trace
import IOTasks.OutputPattern
import IOTasks.ValueMap

interpret :: MonadTeletype m => Specification -> [m ()]
interpret s = do
  collapsed <- collapseChoice s
  pure $ flip evalStateT (Map.empty :: ValueMap) $
    sem
      (\n x (vs :: ValueSet v) m -> RecSub (x,wrapValue . readValue @v ,containsValue vs . unwrapValue,m,n) id (n+1))
      (\case
        RecSub (x,readF,_,AssumeValid,n) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          modify $ insertValue (v,n) x
          p'
        RecSub (x,readF,vsContains,Abort,n) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          if vsContains v
            then do
              modify $ insertValue (v,n) x
              p'
            else lift $ MTT.putStrLn "abort: invalid input value"
        RecSub (x,readF,vsContains,UntilValid,n) () p' -> do
          let readLoop = do
                v <- lift (readF <$> MTT.getLine)
                if vsContains v then pure v else lift (putStrLn "invalid value, try again") >> readLoop
          v <- readLoop
          modify $ insertValue (v,n) x
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
