{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.Interpreter where

import Prelude hiding (readLn,putStrLn)

import Control.Monad.State

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
  pure $ flip evalStateT (emptyValueMap [] :: ValueMap) $
    sem
      (\() x (vs :: ValueSet v) m -> RecSub (x,wrapValue . readValue @v ,containsValue vs . unwrapValue,m) id ())
      (\case
        RecSub (x,readF,_,AssumeValid) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          modify $ insertValue v x
          p'
        RecSub (x,readF,vsContains,Abort) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          if vsContains v
            then do
              modify $ insertValue v x
              p'
            else lift $ MTT.putStrLn "abort: invalid input value"
        RecSub (x,readF,vsContains,UntilValid) () p' -> do
          let readLoop = do
                v <- lift (readF <$> MTT.getLine)
                if vsContains v then pure v else lift (putStrLn "invalid value, try again") >> readLoop
          v <- readLoop
          modify $ insertValue v x
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
