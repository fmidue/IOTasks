module TraceSet where

import Type
import Trace

import Bound.Scope
import Test.QuickCheck hiding (Positive)
import           Data.Either
import           Data.Maybe

traceGen :: Spec VarName -> Gen Trace
traceGen = traceGen' . unsugarT

traceGen' :: Spec VarName -> Gen Trace
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
      return $ ProgWrite (termVal <$> v) t'
    (TillT s s') -> traceGen' (andThen s (JumpPoint s s'))
    (Branch p s11 s12 s2) ->
      if predVal p
        then traceGen' $ andThen s12 s2
        else traceGen' $ andThen s11 s2
    (InternalT (JumpPoint _ s')) -> traceGen' s'
    Nop -> return Stop
    (JumpPoint s s') -> traceGen' $ andThen s (JumpPoint s s')
    _ -> error "not a valid spec"

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

predVal :: Predicate VarName -> Bool
predVal p = fromMaybe (error "not a predicate") (predVal' p)

predVal' :: Predicate VarName -> Maybe Bool
predVal' TT = Just True
predVal' FF = Just False
predVal' (Neg p) = not <$> predVal' p
predVal' (And p q) = (&&) <$> predVal' p <*> predVal' q
predVal' (Or p q) = (||) <$> predVal' p <*> predVal' q
predVal' (Eq t s) = Just $ termVal t == termVal s
predVal' (Less t s) = Just $ termVal t < termVal s
predVal' _ = Nothing

termVal :: Term VarName -> Int
termVal t = fromRight (error "type mismatch") (termVal' t)

termVal' :: Term VarName -> Either [Int] Int
termVal' (Lit v) = Right v
termVal' (Add x y) = (+) <$> termVal' x <*> termVal' y
termVal' (Sub x y) = (-) <$> termVal' x <*> termVal' y
termVal' (Mul x y) = (*) <$> termVal' x <*> termVal' y
termVal'  Nil = Left []
termVal' (Cons x xs) =
  case (termVal' x, termVal' xs) of
    (Right v, Left vs) -> Left $ v : vs
    _ -> error "type mismatch"
termVal' (Sum xs) =
  case termVal' xs of
    Left vs -> Right $ sum vs
    _ -> error "type mismatch"
termVal' (Len xs) =
  case termVal' xs of
    Left vs -> Right $ length vs
    _ -> error "type mismatch"
termVal' (V _) = Left [] -- ASSUMPTION: in "closed" terms we can only encounter accumulator variables and "finalize" them
termVal' t = error $ "not a term: " ++ show t
