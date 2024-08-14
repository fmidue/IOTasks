{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Interpreter (interpret) where

import Prelude hiding (readLn,putStrLn)

import Control.Monad.State

import qualified Data.Set as Set

import Test.IOTasks.Internal.Specification
import Test.IOTasks.MonadTeletype as MTT
import Test.IOTasks.Internal.Term
import Test.IOTasks.Var (someVar)
import Test.IOTasks.ValueSet
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern
import Test.IOTasks.ValueMap

-- | Interpret a specification as a program in 'Monad' m.
--
--   Returns a list containing all possible ways of resolving optionality, e.g.
--
-- >>> length (interpret $ writeOptionalOutput [x,y])
-- 3
interpret :: MonadTeletype m => Specification -> [m ()]
interpret s = do
  collapsed <- collapseChoice s
  pure $ flip evalStateT (emptyValueMap [] :: ValueMap) $
    sem
      -- (\() x (vs :: ValueSet v) m -> RecSub (someVar x,wrapValue . readValue @v, \var m -> (containsValue var m vs . unwrapValue),m) id ())
      (\() x (vs :: ValueSet v) m -> RecSub (someVar x,wrapValue . readValue x, \vMap -> containsValue x vMap vs . unwrapValue x,m) id ())
      (\case
        RecSub (x,readF,_,AssumeValid) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          modify $ insertValue v x
          p'
        RecSub (x,readF,vsContains,ElseAbort) () p' -> do
          v <- lift (readF <$> MTT.getLine)
          vMap <- get
          if vsContains vMap v
            then do
              modify $ insertValue v x
              p'
            else lift $ MTT.putStrLn "abort: invalid input value"
        RecSub (x,readF,vsContains,UntilValid) () p' -> do
          let readLoop = do
                v <- lift (readF <$> MTT.getLine)
                vMap <- get
                if vsContains vMap v then pure v else lift (putStrLn "invalid value, try again") >> readLoop
          v <- readLoop
          modify $ insertValue v x
          p'
        NoRec{} -> error "interpret: impossible"
        RecSame{} -> error "interpret: impossible"
        RecBoth{} -> error "interpret: impossible")
      (\_ opt ts p' -> case opt of
        Mandatory -> do
          if Set.size ts == 1
            then do
              e <- get
              lift $ putStrLn $ showPattern $ snd $ evalPattern e (Set.elemAt 0 ts)
              p'
            else error "interpret: impossible (due to collapseChoice)"
        Optional -> error "interpret: impossible (due to collapseChoice)"
      )
      (\_ cond pl pr -> do
        e <- get
        if snd $ oEval e cond
          then pl
          else pr
      )
      (const id)
      id
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
