{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Translation (
  buildComputation
) where

import Test.IOTest.Environment
import Test.IOTest.Pattern
import Test.IOTest.ValueSet
import Test.IOTest.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Specification

import Data.Maybe
import Data.Proxy
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State

import Type.Reflection

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: 'throwError Exit' at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: MonadTeletype m => Specification -> Semantics m ()
buildComputation' = interpret buildRead buildWrite

buildRead :: MonadTeletype m => Action -> Semantics m ()
buildRead (ReadInput x vs) =
  withProxy vs $ \(_ :: Proxy ty) -> do
      v <- unpack @ty <$> getLine
      unless (containsValue vs (Value typeRep v)) (error "encountered out of range input")
      modify (fromJust . update x v)
buildRead _ = error "buildRead"

buildWrite :: MonadTeletype m => Action -> Semantics m ()
buildWrite (WriteOutput _ [] _) = error "empty list of output options"
buildWrite (WriteOutput True _ _) =
  mempty
buildWrite (WriteOutput False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . render . pPrint $ v
  where
    eval = fillHoles
buildWrite _ = error "buildWrite"
