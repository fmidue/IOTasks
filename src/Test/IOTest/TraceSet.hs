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

traceGen :: MonadGen m => Specification -> m GeneralizedNTrace
traceGen (Spec s) = traceSet s kTI (freshEnvironment (Spec s)) where
  kTI End  _ = return . GNT $ NonWrite StopN
  kTI Exit _ = error "traceGen: 'throwError Exit' at toplevel"

traceSet :: MonadGen m => [Action] -> (Cont -> Environment -> m GeneralizedNTrace) -> Environment -> m GeneralizedNTrace
traceSet (ReadInput x ty : s') k e = do
  v <- valueOf ty
  let e' = fromMaybe (error "type mismatch on environment update") (updateWithValue x v e)
  fmap (progReadN (show v) <>) (traceSet s' k e')
traceSet (WriteOutput opt ps ts : s') k e =
  let v1 = S.fromList ((\p -> fillHoles (p,ts) e) <$> ps)
      v1' = if opt then S.insert emptyPattern v1 else v1
  in fmap (v1' <.>) (traceSet s' k e)
traceSet (Branch c (Spec s1) (Spec s2) : s') k e = if evalTerm c e then traceSet (s2 <> s') k e else traceSet (s1 <> s') k e
traceSet (TillE (Spec s) : s') k e = traceSet s k' e
  where k' End  = traceSet s k'
        k' Exit = traceSet s' k
traceSet (E : s') k e = k Exit e
traceSet [] k e = k End e

data Cont = Exit | End

(<.>) :: S.Set Pattern -> GeneralizedNTrace -> GeneralizedNTrace
v <.> (GNT (ProgWriteN v' t'')) = GNT $ ProgWriteN (v `langConcat` v') t''
v <.> (GNT (NonWrite t')) = GNT $ ProgWriteN v t'

langConcat :: S.Set Pattern -> S.Set Pattern -> S.Set Pattern
langConcat xs ys = S.map (uncurry (<>)) $ S.cartesianProduct xs ys

sampleTrace ::  MonadGen m => GeneralizedTrace -> m OrdinaryTrace
sampleTrace (GT (ProgRead v t)) = do
  (OT t') <- sampleTrace (GT t)
  return $ OT $ ProgRead v t'
sampleTrace (GT Stop) = return $ OT Stop
sampleTrace (GT OutOfInputs) = return $ OT OutOfInputs
sampleTrace (GT (ProgWrite vs t2)) = do
  p <- elements $ S.toList vs
  v <- extract p
  (OT t1) <- sampleTrace (GT t2)
  return $ if p == emptyPattern
    then OT t1
    else OT $ ProgWrite v t1

-- only works as intended if the input trace has linebreaks only after each previously seperate output
sampleNTrace ::  MonadGen m => GeneralizedNTrace -> m OrdinaryTrace
sampleNTrace (GNT (NonWrite (ProgReadN v t))) = do
  (OT t') <- sampleNTrace (GNT t)
  return $ OT $ ProgRead v t'
sampleNTrace (GNT (NonWrite StopN)) = return $ OT Stop
sampleNTrace (GNT (NonWrite OutOfInputsN)) = return $ OT OutOfInputs
sampleNTrace (GNT (ProgWriteN vs t2)) = do
  p <- elements $ S.toList vs
  v <- fmap (++ "\n") . lines <$> extract p
  (OT t1) <- sampleNTrace (GNT $ NonWrite t2)
  return $ if p == emptyPattern
    then OT t1
    else OT $ foldr ProgWrite Stop v <> t1
