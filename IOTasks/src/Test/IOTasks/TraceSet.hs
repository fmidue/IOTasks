{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTasks.TraceSet (
  traceGen,
  sampleNTrace,
) where

import Prelude hiding (putStrLn, getLine, GT)

import Test.IOTasks.Trace
import Test.IOTasks.Pattern
import Data.Environment
import Test.IOTasks.ValueSet
import Test.IOTasks.Specification
import Data.Term (SemTerm(..), VarListTerm, evalTerm)

import Test.QuickCheck.GenT

import qualified Data.Set as S
import           Data.Maybe
import           Control.Monad                  ( ap )

traceGen :: forall t m. (MonadGen m, SemTerm t (Environment Varname), VarListTerm t Varname) => Specification t -> m GeneralizedTrace
traceGen (Spec s) = chooseTrace valueOf $ traceSet s kTI (freshEnvironment $ specVars (Spec s)) where

kTI :: Applicative f => Cont -> a -> f GeneralizedTrace
kTI End  _ = pure StopN
kTI Exit _ = error "traceGen: 'throwError Exit' at toplevel"

traceSet :: SemTerm t (Environment Varname) => [Action (Specification t) t] -> (Cont -> Environment Varname -> TraceTree) -> Environment Varname -> TraceTree
traceSet (ReadInput x ty : s') k e =
  Bind $ ValueChoice ty $ \v ->
    let e' = fromMaybe (error "type mismatch on environment update") (storeValue x v e)
    in fmap (ProgReadN (show v) StopN <>) (traceSet s' k e')
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

(<.>) :: MergeSet FixedPattern -> GeneralizedTrace -> GeneralizedTrace
vs <.> ProgWriteReadN vs' v' t'' = ProgWriteReadN (langConcat vs vs') v' t''
vs <.> ProgWriteStopN vs' = ProgWriteStopN (langConcat vs vs')
v <.> t' = ProgWriteStopN v <> t'

langConcat :: MergeSet FixedPattern -> MergeSet FixedPattern -> MergeSet FixedPattern
langConcat = (<>)

-- | only works as intended if the input trace has linebreaks after each previously seperate output
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

-- new implementation
data Free f a
  = Return a
  | Bind (f (Free f a))
  deriving Functor

instance (Functor f) => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  return = Return
  (Return a) >>= f = f a
  (Bind m) >>= f = Bind ((>>= f) <$> m)

data ValueChoice a = ValueChoice { valueSet :: ValueSet , chooseValue :: Value -> a }
  deriving Functor

type TraceTree = Free ValueChoice GeneralizedTrace

chooseTrace :: (Monad m) => (ValueSet -> m Value) -> TraceTree -> m GeneralizedTrace
chooseTrace f = chooseTrace' (const f)

chooseTrace' :: (Monad m) => (Int -> ValueSet -> m Value) -> TraceTree -> m GeneralizedTrace
chooseTrace' f = go 0 where
  go _ (Return t) = pure t
  go i (Bind (ValueChoice ty cont)) = f i ty >>= (go (i+1) . cont)

genTraceForSequence :: (SemTerm t (Environment Varname), VarListTerm t Varname) => [Value] -> Specification t -> Maybe GeneralizedTrace
genTraceForSequence is (Spec as) = chooseTrace' maybeChoose $ traceSet as kTI (freshEnvironment $ specVars (Spec as))
  where
    maybeChoose i ty =
      let v = (is !! i)
      in if ty `containsValue` v then Just v else Nothing
