{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Translation (
  buildComputation,
  buildWrongComputation,
) where

import Test.IOTest.Environment
import Test.IOTest.Pattern
import Test.IOTest.ValueSet
import Test.IOTest.Utils
import Test.IOTest.Term

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Specification

import Data.Maybe
import Data.Proxy
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State
import Control.Monad.Extra ( ifM )

import Type.Reflection
import GHC.TypeLits

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: 'throwError Exit' at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: MonadTeletype m => Specification -> Semantics m ()
buildComputation' = interpret build

build :: MonadTeletype m => Action -> Semantics m ()
build (ReadInput x vs) =
  withProxy vs $ \(_ :: Proxy ty) -> do
      v <- unpack @ty <$> getLine
      unless (containsValue vs (Value typeRep v)) (error "encountered out of range input")
      modify (fromJust . update x v)
build (WriteOutput _ [] _) = error "empty list of output options"
build (WriteOutput True _ _) =
  mempty
build (WriteOutput False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . render . pPrint $ v
  where
    eval = fillHoles
build _ = return ()

buildWrongComputation :: MonadTeletype m => Specification -> m ()
buildWrongComputation s = do
  loopStatus <- evalSemantics (buildWrongComputation' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildWrongComputation: 'throwError Exit' at toplevel"

buildWrongComputation' :: MonadTeletype m => Specification -> Semantics m ()
buildWrongComputation' = interpret' buildWrong

buildWrong :: MonadTeletype m => (Action, Semantics m ()) -> Semantics m ()
buildWrong (ReadInput{} ,p) = p
buildWrong (WriteOutput{} ,p) = p
buildWrong (Branch c s1 s2, _) =
  ifM (gets (evalTerm c))
    -- swap branches
    (buildWrongComputation s2)
    (buildWrongComputation s1)
buildWrong (TillE _, p) = p
buildWrong (E,p) = p
