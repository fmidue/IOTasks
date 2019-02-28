{-# LANGUAGE LambdaCase #-}
module TraceSet where

import Type
import Trace
import Context
import qualified Language as Surface
import Language (Function,NumType(..))

import Test.QuickCheck hiding (Positive,Function)
import           Control.Arrow
import qualified Data.Set as S

traceGen :: Surface.Specification -> Gen (NTrace Int)
traceGen = uncurry traceGen' . (freshContext &&& id) . unsugar

traceGen' :: Context -> Spec -> Gen (NTrace Int)
traceGen' d spec = sized $ \size ->
  case spec of
    (Read x ty s') -> do
      v <- genInt ty
      t' <- traceGen' (update d x v) s'
      return $ ProgWrite (S.singleton []) $ ProgRead v t'
    (Write ts s') -> do
      let v1 = S.fromList $ (if isOptional ts then ([]:) else id) $ pure . evalF d <$> filterEpsilon ts
      t <- traceGen' d s'
      let (ProgWrite v2 t') = t
      let v = S.map (uncurry (++)) $ S.cartesianProduct v1 v2
      return $ ProgWrite v t'
    (TillT s s') -> traceGen' d (andThen s (JumpPoint s s'))
    (Branch p s11 s12 s2) ->
      if evalP d p
        then traceGen' d $ andThen s12 s2
        else traceGen' d $ andThen s11 s2
    (InternalT (JumpPoint _ s')) -> traceGen' d s'
    Nop -> return $ ProgWrite (S.singleton []) Stop
    (JumpPoint s s') -> traceGen' d $ andThen s (JumpPoint s s')
    _ -> error "not a valid spec"

isOptional :: [Function] -> Bool
isOptional [] = False
isOptional (Surface.Optional:_) = True
isOptional (_:xs) = isOptional xs

filterEpsilon :: [Function] -> [Function]
filterEpsilon = filter (\case Surface.Optional -> False; _ -> True)

genInt :: NumType -> Gen Int
genInt IntTy = choose (-10,10)
genInt NatTy = choose (0,10)
genInt Positive = choose (1,10)
genInt Negative = choose (-10,-1)
genInt Zero = return 0
genInt (Not IntTy) = error "empty type"
genInt (Not NatTy) = genInt Negative
genInt (Not Positive) = choose (-10,0)
genInt (Not Negative) = genInt NatTy
genInt (Not Zero) = oneof [genInt Negative, genInt Positive]
genInt (Not (Not ty)) = genInt ty
