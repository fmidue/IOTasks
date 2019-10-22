{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.TraceSet (
  traceGen,
  sampleTrace,
) where

import Prelude hiding (putStrLn, getLine, GT)

import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Trace
import Test.IOTest.Pattern
import Test.IOTest.Environment
import Test.IOTest.ValueSet
import Test.IOTest.Specification

-- import Test.QuickCheck hiding (Positive,Function)
import Test.QuickCheck.GenT

import qualified Data.Set as S
import           Data.Maybe

import System.Random

import Control.Monad.State
import Control.Monad.Writer

traceGen :: MonadGen m => Specification -> m GeneralizedTrace
traceGen s = do
  (loopStatus, t) <- runWriterT $ evalSemantics (traceGen' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return t
    Left Exit -> error "traceGen: 'throwError Exit' at toplevel"

traceGen' :: (MonadGen m, MonadWriter GeneralizedTrace m) => Specification -> Semantics m ()
traceGen' = interpret genRead genWrite

genRead :: (MonadGen m, MonadWriter GeneralizedTrace m) => Action -> Semantics m ()
genRead (ReadInput x vs) = sized $ \size -> do
  seed <- choose (minBound, maxBound)
  let val = valueOf vs (mkStdGen seed)
  modify (fromMaybe (error "type mismatch on environment update") . updateWithValue x val)
  tell $ progReadG (show val)
  -- FIXME: clean up according to paper definition?
genRead _ = error "genRead"

genWrite :: (MonadGen m, MonadWriter GeneralizedTrace m) => Action -> Semantics m ()
genWrite (WriteOutput opt ps ts) = sized $ \size -> do
  d <- get
  let v1 = S.fromList ((\p -> fillHoles (p,ts) d) <$> ps)
      v1' = if opt then S.insert emptyPattern v1 else v1
  unless (null v1') $ tell (progWriteG v1')
genWrite _ = error "genWrite"

sampleTrace ::  MonadGen m => GeneralizedTrace -> m OrdinaryTrace
sampleTrace (GT (ProgRead v t)) = do
  (OT t') <- sampleTrace (GT t)
  return $ OT $ ProgRead v t'
sampleTrace (GT Stop) = return $ OT Stop
sampleTrace (GT OutOfInputs) = return $ OT OutOfInputs
sampleTrace (GT (ProgWrite vs t2)) = do
  p <- elements $ S.toList vs
  seed <- choose (minBound, maxBound)
  let v = extract (mkStdGen seed) p
  (OT t1) <- sampleTrace (GT t2)
  return $ if p == emptyPattern
    then OT t1
    else OT $ ProgWrite v t1
