{-# LANGUAGE LambdaCase #-}
module Test.IOTest.TraceSet (
  traceGen
) where

import Test.IOTest.Type
import Test.IOTest.Trace
import Test.IOTest.Context
import qualified Test.IOTest.Language as Surface
import Test.IOTest.Language (Function,NumType(..),VarName)

import Test.QuickCheck hiding (Positive,Function)
import           Control.Arrow
import qualified Data.Set as S

traceGen :: Surface.Specification VarName -> Gen (NTrace Int)
traceGen = uncurry traceGen' . (freshContext &&& id) . unsugar

traceGen' :: Context VarName -> Spec -> Gen (NTrace Int)
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
    (TillE s s') -> traceGen' d (andThen s (JumpPoint s s'))
    (Branch p s11 s12 s2) ->
      if evalP d p
        then traceGen' d $ andThen s12 s2
        else traceGen' d $ andThen s11 s2
    (InternalE (JumpPoint _ s')) -> traceGen' d s'
    Nop -> return $ ProgWrite (S.singleton []) Stop
    (JumpPoint s s') -> traceGen' d $ andThen s (JumpPoint s s')
    _ -> error "not a valid spec"

isOptional :: [Function a] -> Bool
isOptional [] = False
isOptional (Surface.Optional:_) = True
isOptional (_:xs) = isOptional xs

filterEpsilon :: [Function a] -> [Function a]
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
