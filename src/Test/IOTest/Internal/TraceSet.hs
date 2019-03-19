{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Internal.TraceSet (
  traceGen
) where

import Test.IOTest.Internal.InternalSpec
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term hiding (update)
import Test.IOTest.Internal.Context
import qualified Test.IOTest.Internal.Specification as Surface
import Test.IOTest.Internal.Specification (Restrictable(..),VarName)

import Test.QuickCheck hiding (Positive,Function)
import           Control.Arrow
import qualified Data.Set as S

traceGen :: (Ord a, Normalizeable a, Restrictable a) => Surface.Specification VarName a -> Gen (NTrace a)
traceGen = uncurry traceGen' . (freshContext &&& id) . unsugar

traceGen' :: (Ord a, Normalizeable a, Restrictable a) => Context VarName a -> Spec a -> Gen (NTrace a)
traceGen' d spec = sized $ \size ->
  case spec of
    (Read x ty s') -> do
      v <- restrict ty
      t' <- traceGen' (update d x v) s'
      return $ ProgWrite (S.singleton []) $ ProgRead v t'
    (Write ts s') -> do
      let v1 = S.fromList $ (\t -> if isEpsilon t then [] else pure . toPattern $ evalTerm t d) <$> ts
      t <- traceGen' d s'
      let (ProgWrite v2 t') = t
      let v = S.map (uncurry (++)) $ S.cartesianProduct v1 v2
      return $ ProgWrite v t'
    (WriteP p s') -> do
      let cPs =
            (\case
              (Exactly t desc) -> [ExactlyC (evalTerm t d) desc]
              (Contains t desc) -> [ContainsC (evalTerm t d) desc]
              Everything -> [EverythingC]
              NoOutput -> []
            ) <$> p
      let v1 = S.fromList cPs
      t <- traceGen' d s'
      let (ProgWrite v2 t') = t
      let v = S.mapMonotonic (uncurry (++)) $ S.cartesianProduct v1 v2
      return $ ProgWrite v t'
    (TillE s s') -> traceGen' d (andThen s (JumpPoint s s'))
    (Branch p s11 s12 s2) ->
      if evalTerm p d
        then traceGen' d $ andThen s12 s2
        else traceGen' d $ andThen s11 s2
    (InternalE (JumpPoint _ s')) -> traceGen' d s'
    Nop -> return $ ProgWrite (S.singleton []) Stop
    (JumpPoint s s') -> traceGen' d $ andThen s (JumpPoint s s')
    (InternalE _) -> error "not a valid spec"
