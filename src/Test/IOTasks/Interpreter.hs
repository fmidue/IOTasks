{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTasks.Interpreter (interpret) where

import Prelude hiding (readLn,putStrLn)

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
  pure $
    semSt
      (\x (vs :: ValueSet v) m res e ->
        let
          xVar = someVar x
          readF = wrapValue . readValue x
          vsContains vMap = containsValue x vMap vs . unwrapValue x
          p' = subRes res
        in case m of
          AssumeValid -> do
            v <- readF <$> MTT.getLine
            p' $ insertValue v xVar e
          ElseAbort -> do
            v <- readF <$> MTT.getLine
            if vsContains e v
              then do
                p' $ insertValue v xVar e
              else MTT.putStrLn "abort: invalid input value"
          UntilValid -> do
            let readLoop = do
                  v <- readF <$> MTT.getLine
                  if vsContains e v then pure v else putStrLn "invalid value, try again" >> readLoop
            v <- readLoop
            p' $ insertValue v xVar e
      )
      (\opt ts p' e -> case opt of
        Mandatory ->
          if Set.size ts == 1
            then do
              putStrLn $ showPattern $ snd $ evalPattern e (Set.elemAt 0 ts)
              p'
            else error "interpret: impossible (due to collapseChoice)"
        Optional -> error "interpret: impossible (due to collapseChoice)"
      )
      (\cond pl pr e ->
        if snd $ oEval e cond
          then pl
          else pr
      )
      id
      (pure ())
      (emptyValueMap $ readVars collapsed)
      collapsed

collapseChoice :: Specification -> [Specification]
collapseChoice (ReadInput x vs m s') = ReadInput x vs m <$> collapseChoice s'
collapseChoice (WriteOutput o ts s') = do
  WriteOutput Mandatory <$> (Set.singleton <$> Set.toList ts) <*> collapseChoice s' ++ if o == Optional then collapseChoice s' else []
collapseChoice (Branch p l r s') = Branch p <$> collapseChoice l <*> collapseChoice r <*> collapseChoice s'
collapseChoice (TillE bdy s') = TillE <$> collapseChoice bdy <*> collapseChoice s'
collapseChoice Nop = pure Nop
collapseChoice E = pure E
