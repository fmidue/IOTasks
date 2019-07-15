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
import qualified Data.Set as S

traceGen :: (Ord a, Normalizeable a, Restrictable a) => Surface.Specification VarName a -> Gen (NTrace a)
traceGen = (\s -> traceGen' s kI (freshContext s)) . unsugar
  where kI Continue = const . return $ ProgWrite (S.singleton []) Stop
        kI Exit = error "ill formed spec!"
traceGen' :: (Ord a, Normalizeable a, Restrictable a)
  -- specification
  => Spec a
  -- contiuation
  -> (Action -> Context VarName a -> Gen (NTrace a))
  -- variable context
  -> Context VarName a
  -> Gen (NTrace a)
traceGen' spec k d = sized $ \size ->
  case spec of
    (Read x ty s') -> do
      v <- restrict ty
      t' <- traceGen' s' k (update d x v)
      return $ ProgWrite (S.singleton []) $ ProgRead v t'
      -- FIXME: clean up according to paper definition?
    (Write ts s') -> do
      let v1 = S.fromList $ (\t -> if isEpsilon t then [] else pure . toPattern $ evalTerm t d) <$> ts
      t <- traceGen' s' k d
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
      t <- traceGen' s' k d
      let (ProgWrite v2 t') = t
      let v = S.mapMonotonic (uncurry (++)) $ S.cartesianProduct v1 v2
      return $ ProgWrite v t'
    (TillE s s') ->
      let k' Continue = traceGen' s k'
          k' Exit = traceGen' s' k
      in traceGen' s k' d
    (Branch p s11 s12 s2) ->
      if evalTerm p d
        then traceGen' (andThen s12 s2) k d
        else traceGen' (andThen s11 s2) k d
    (InternalE _) -> k Exit d
    Nop -> k Continue d

data Action = Continue | Exit deriving (Eq,Show)
