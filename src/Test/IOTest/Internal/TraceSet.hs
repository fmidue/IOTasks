{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Internal.TraceSet (
  traceGen
) where

import Test.IOTest.Internal.InternalSpec
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.ValueSet
import qualified Test.IOTest.Internal.Specification as Surface

import Test.QuickCheck hiding (Positive,Function)
import qualified Data.Set as S
import           Data.Maybe
import           System.Random

traceGen :: Surface.Specification -> Gen NTrace
traceGen = (\s -> traceGen' s kI (freshContext s)) . unsugar
  where kI Continue = const . return $ ProgWrite (S.singleton emptyPattern) Stop
        kI Exit = error "ill formed spec!"

traceGen' ::
  -- specification
     Spec
  -- contiuation
  -> (Action -> Context -> Gen NTrace)
  -- variable context
  -> Context
  -> Gen NTrace
traceGen' spec k d = sized $ \size ->
  case spec of
    (Read x vs s') -> do
      seed <- choose (minBound, maxBound)
      let v = valueOf vs (mkStdGen seed) -- TODO: is there a better way of doing this?
      t' <- traceGen' s' k (fromMaybe (error "type mismatch on context update") $ update x v d)
      return $ ProgWrite (S.singleton emptyPattern) $ ProgRead (show v) t'
      -- FIXME: clean up according to paper definition?
    (Write pxy opt ps ts s') -> do
      let v1 = S.fromList ((\p -> fillHoles pxy p ts d) <$> ps)
          v1' = if opt then S.insert emptyPattern v1 else v1
      t <- traceGen' s' k d
      let (ProgWrite v2 t') = t
      let v = S.map (uncurry (<>)) $ S.cartesianProduct v1' v2
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
