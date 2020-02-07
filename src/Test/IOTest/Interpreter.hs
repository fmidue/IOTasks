{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Interpreter (
  buildComputation,
  -- buildWrongComputation,
) where

import Test.IOTest.Environment
import Test.IOTest.Pattern
import Test.IOTest.ValueSet
import Test.IOTest.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Specification
import Test.IOTest.Term (SemTerm(..),TermVars)

import Data.Maybe
import Data.Proxy
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State
import Control.Monad.Extra ( ifM )

import Type.Reflection

buildComputation :: (SemTerm t, TermVars t) => MonadTeletype m => Specification t -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment (specVars s))
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: 'throwError Exit' at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: (MonadTeletype m, SemTerm t) => Specification t -> Semantics m ()
buildComputation' = interpret (build InputRangeCheck)

data Checks = NoCheck | InputRangeCheck deriving (Eq, Show)

build :: (MonadTeletype m, SemTerm t) => Checks -> Action t -> Semantics m ()
build c (ReadInput x vs) =
  withProxy vs $ \(_ :: Proxy ty) -> do
      v <- unpack @ty <$> getLine
      when (c == InputRangeCheck && not (containsValue vs (Value typeRep v)))
        (error "encountered out of range input")
      modify (fromJust . store x v)
build _ (WriteOutput _ [] _) = error "empty list of output options"
build _ (WriteOutput True _ _) =
  mempty
build _ (WriteOutput False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . render . pPrint $ v
  where
    eval = fillHoles
build _ _ = error "not a read or write action"

buildWrongComputation :: (MonadTeletype m, SemTerm t, TermVars t) => Specification t -> m ()
buildWrongComputation s = do
  loopStatus <- evalSemantics (buildWrongComputation' s) (freshEnvironment (specVars s))
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildWrongComputation: 'throwError Exit' at toplevel"

buildWrongComputation' :: (MonadTeletype m, SemTerm t, TermVars t) => Specification t -> Semantics m ()
buildWrongComputation' = interpret' buildWrong

buildWrong :: (MonadTeletype m, SemTerm t, TermVars t) => (Action t, Semantics m ()) -> Semantics m ()
buildWrong (r@ReadInput{}, _) = build NoCheck r
buildWrong (w@WriteOutput{}, _) = build NoCheck w
buildWrong (Branch c s1 s2, _) =
  ifM (gets (evalTerm c))
    -- swap branches
    (buildWrongComputation s2)
    (buildWrongComputation s1)
buildWrong (TillE _, p) = p
buildWrong (E,p) = p
