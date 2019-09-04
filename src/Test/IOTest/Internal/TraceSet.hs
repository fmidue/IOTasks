{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Internal.TraceSet (
  traceGen
) where

import Prelude hiding (putStrLn, getLine)

import Test.IOTest.Semantics
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Internal.Specification

-- import Test.QuickCheck hiding (Positive,Function)
import Test.QuickCheck.GenT
import qualified Data.Set as S
import           Data.Maybe

import System.Random

import Control.Monad.State
import Control.Monad.Writer

traceGen :: MonadGen m => Specification -> m NTrace
traceGen s = do
  (loopStatus, t) <- runWriterT $ evalSemantics (traceGen' s) (freshContext s)
  case loopStatus of
    Just () -> return t
    Nothing -> error "traceGen: loopEnd at toplevel"

traceGen' :: (MonadGen m, MonadWriter NTrace m) => Specification -> Semantics m ()
traceGen' = interpret genRead genWrite

genRead :: (MonadGen m, MonadWriter NTrace m) => Action -> Semantics m ()
genRead (ReadInput x vs) = sized $ \size -> do
  seed <- choose (minBound, maxBound)
  let v = valueOf vs (mkStdGen seed) -- TODO: is there a better way of doing this?
  modify (fromMaybe (error "type mismatch on context update") . update x v)
  tell $ Trace [ProgRead (show v)]
  -- FIXME: clean up according to paper definition?
genRead _ = error "genRead"

genWrite :: (MonadGen m, MonadWriter NTrace m) => Action -> Semantics m ()
genWrite (WriteOutput pxy opt ps ts) = sized $ \size -> do
  d <- get
  let v1 = S.fromList ((\p -> fillHoles pxy p ts d) <$> ps)
      v1' = if opt then S.insert emptyPattern v1 else v1
  unless (null v1') $ tell (Trace [ProgWrite v1'])
genWrite _ = error "genWrite"
