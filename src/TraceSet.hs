{-# LANGUAGE LambdaCase #-}
module TraceSet where

import Type
import Trace
import qualified Language as Surface
import Language (VarName,NumType(..))

import Bound.Scope
import Test.QuickCheck hiding (Positive)
import           Data.Either
import           Data.Maybe
import           Data.Bifunctor
import           Control.Monad

traceGen :: Surface.Specification -> Gen (GTrace Int)
traceGen = traceGen' . unsugar

traceGen' :: Spec VarName -> Gen (GTrace Int)
traceGen' spec = sized $ \size ->
  case spec of
    (Read xs ty s') -> do
      v <- genInt ty
      let s'' = instantiateEither sigma s'
          sigma (Left ()) = Lit v
          sigma (Right ys) = if xs == ys then Cons (Lit v) (V xs) else V ys
      t' <- traceGen' s''
      return $ ProgRead v t'
    (Write v s') -> do
      t' <- traceGen' s'
      let oList = (if isOptional v then Can else Must) (filterEpsilon v)
      return $ ProgWrite (termVal <$> oList) t'
    (TillT s s') -> traceGen' (andThen s (JumpPoint s s'))
    (Branch p s11 s12 s2) ->
      if predVal p
        then traceGen' $ andThen s12 s2
        else traceGen' $ andThen s11 s2
    (InternalT (JumpPoint _ s')) -> traceGen' s'
    Nop -> return Stop
    (JumpPoint s s') -> traceGen' $ andThen s (JumpPoint s s')
    _ -> error "not a valid spec"

isOptional :: [Type.Fun a] -> Bool
isOptional [] = False
isOptional (DoNothing:_) = True
isOptional (_:xs) = isOptional xs

filterEpsilon :: [Type.Fun a] -> [Type.Fun a]
filterEpsilon = filter (\case DoNothing -> False; _ -> True)

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

predVal :: Pred VarName -> Bool
predVal p = fromMaybe (error $ "not a predicate: " ++ show p) (predVal' p)

predVal' :: Pred VarName -> Maybe Bool
predVal' (UPred (UIntP p) x) = applyToRight p (termVal' x)
predVal' (BPred (BIntP p) x y) = join $ applyToRight (\v1 -> applyToRight (p v1) (termVal' y)) (termVal' x)
predVal' (UPred (UListP p) x) = applyToLeft p (termVal' x)
predVal' (BPred (BListP p) x y) = join $ applyToLeft (\v1 -> applyToLeft (p v1) (termVal' y)) (termVal' x)
predVal' (BPred (MixedP p) x y) = join $ applyToLeft (\v1 -> applyToRight (p v1) (termVal' y)) (termVal' x)
predVal' _ = Nothing

applyToRight :: (b -> c) -> Either a b -> Maybe c
applyToRight f = either noResult (Just . f)

applyToLeft :: (a -> c) -> Either a b -> Maybe c
applyToLeft f = either (Just . f) noResult

noResult :: b -> Maybe a
noResult = const Nothing

termVal :: Value VarName -> Int
termVal t = fromRight (error "type mismatch") (termVal' t)

termVal' :: Value VarName -> Either [Int] Int
termVal' (Lit v) = Right v
termVal'  Nil = Left []
termVal' (V _) = Left [] -- ASSUMPTION: in "closed" terms we can only encounter accumulator variables and "finalize" them
termVal' (Cons x xs) =
  case (termVal' x, termVal' xs) of
    (Right v, Left vs) -> Left $ v : vs
    _ -> error "type mismatch"
termVal' (UFun (UIntF f) x) = bimap typeError f $ termVal' x
termVal' (BFun (BIntF f) x y) = bimap typeError (\v1 -> either typeError (f v1) (termVal' y)) (termVal' x)
termVal' (UFun (UListF f) x) = either (Right . f) typeError (termVal' x)
termVal' (BFun (BListF f) x y) = either (\v1 -> either (Right . f v1) typeError (termVal' y)) typeError (termVal' x)
termVal' (BFun (MixedF f) x y) = either (\v1 -> either typeError (Right . f v1) (termVal' y)) typeError (termVal' x)
termVal' t = error $ "not a term: " ++ show t

typeError :: p -> a
typeError _ = error "type mismatch"
