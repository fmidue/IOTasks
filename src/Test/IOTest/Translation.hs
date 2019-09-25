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

import Test.IOTest.Internal.Environment
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Internal.Specification

import Data.Maybe
import System.Random
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: loopExit at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: MonadTeletype m => Specification -> Semantics m ()
buildComputation' = interpret buildRead buildWrite

buildRead :: MonadTeletype m => Action -> Semantics m ()
buildRead (ReadInput x vs) =
  elimValueSet vs (error "proxy RandomGen sampled" :: StdGen)
    (\ p _ (_ :: ty) -> do
      v <- unpack @_ @ty p <$> getLine
      modify (fromJust . update x (Value p v))
    )
buildRead _ = error "buildRead"

buildWrite :: MonadTeletype m => Action -> Semantics m ()
buildWrite (WriteOutput _ _ [] _) = error "empty list of output options"
buildWrite (WriteOutput _ True _ _) =
  mempty
buildWrite (WriteOutput pxy False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . render . pPrint $ v
  where
    eval = fillHoles pxy
buildWrite _ = error "buildWrite"
