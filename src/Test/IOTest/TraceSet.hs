{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.TraceSet (
  traceGen,
  sampleNTrace,
) where

import Prelude hiding (putStrLn, getLine, GT)

import Test.IOTest.Trace
import Test.IOTest.Pattern
import Test.IOTest.Environment
import Test.IOTest.ValueSet
import Test.IOTest.Specification
import Test.IOTest.Term

import Test.QuickCheck.GenT

import qualified Data.Set as S
import           Data.Maybe
import           Data.Function                  ( on )

traceGen :: MonadGen m => Specification -> m GeneralizedTrace
traceGen (Spec s) = traceSet s kTI (freshEnvironment (Spec s)) where
  kTI End  _ = return StopN
  kTI Exit _ = error "traceGen: 'throwError Exit' at toplevel"

traceSet :: MonadGen m => [Action] -> (Cont -> Environment -> m GeneralizedTrace) -> Environment -> m GeneralizedTrace
traceSet (ReadInput x ty : s') k e = do
  v <- valueOf ty
  let e' = fromMaybe (error "type mismatch on environment update") (updateWithValue x v e)
  fmap (ProgReadN (show v) StopN <>) (traceSet s' k e')
traceSet (WriteOutput opt ps ts : s') k e =
  let v1 = S.fromList ((\p -> fillHoles (p,ts) e) <$> ps)
      v1' = MkMergeSet $ if opt then S.insert emptyPattern v1 else v1
  in fmap (v1' <.>) (traceSet s' k e)
traceSet (Branch c (Spec s1) (Spec s2) : s') k e = if evalTerm c e then traceSet (s2 <> s') k e else traceSet (s1 <> s') k e
traceSet (TillE (Spec s) : s') k e = traceSet s k' e
  where k' End  = traceSet s k'
        k' Exit = traceSet s' k
traceSet (E : s') k e = k Exit e
traceSet [] k e = k End e

data Cont = Exit | End

(<.>) :: MergeSet Pattern -> GeneralizedTrace -> GeneralizedTrace
vs <.> ProgWriteReadN vs' v' t'' = ProgWriteReadN (langConcat vs vs') v' t''
vs <.> ProgWriteStopN vs' = ProgWriteStopN (langConcat vs vs')
v <.> t' = ProgWriteStopN v <> t'

langConcat :: MergeSet Pattern -> MergeSet Pattern -> MergeSet Pattern
langConcat xs ys = MkMergeSet $ S.map (uncurry (<>)) $ (S.cartesianProduct `on` fromMergeSet) xs ys

-- sampleTrace ::  MonadGen m => GeneralizedTrace -> m OrdinaryTrace
-- sampleTrace (ProgReadN v t) = do
--   t' <- sampleTrace t
--   return $ ProgRead v t'
-- sampleTrace StopN = return Stop
-- sampleTrace (ProgWriteReadN vs v t2) = do
--   p <- elements $ S.toList vs
--   v <- extract p
--   t1 <- sampleTrace t2
--   return $ if p == emptyPattern
--     then OT t1
--     else OT $ ProgWrite v t1

-- only works as intended if the input trace has linebreaks only after each previously seperate output
sampleNTrace ::  MonadGen m => GeneralizedTrace -> m OrdinaryTrace
sampleNTrace (ProgReadN v t) = do
  t' <- sampleNTrace t
  return $ ProgRead v t'
sampleNTrace StopN = return Stop
sampleNTrace (ProgWriteStopN (MkMergeSet vs)) = do
  p <- elements $ S.toList vs
  vs' <- fmap (++ "\n") . lines <$> extract p
  return $ if p == emptyPattern
    then Stop
    else foldr ProgWrite Stop vs'
sampleNTrace (ProgWriteReadN (MkMergeSet vs) v t2) = do
  p <- elements $ S.toList vs
  vs' <- fmap (++ "\n") . lines <$> extract p
  t1 <- sampleNTrace t2
  return $ if p == emptyPattern
    then ProgRead v t1
    else foldr ProgWrite Stop vs' <> ProgRead v t1
