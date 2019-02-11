module Matching where

import Type
import IOtt

import Bound.Scope
import Data.Either
import Data.Maybe
import Control.Monad.Free
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class (lift)
import Test.QuickCheck

-- matching against full Traces
matchesFull :: Trace' Int () -> Spec VarName -> Either String ()
matchesFull t s = matchesFull' t (unsugarT s)

matchesFull' :: Trace' Int () -> Spec VarName -> Either String ()
matchesFull' (Finish ()) Nop = Right ()
matchesFull' (ProgRead v t') (Read xs s') =
  let s'' = instantiateEither sigma s'
      sigma (Left ()) = Lit v
      sigma (Right ys) = if xs == ys then Cons (Lit v) (V xs) else V ys
  in t' `matchesFull'` s''
matchesFull' (ProgWrite v t') (Write t s') =
  if v /= termVal t
    then Left ("Equaltiy test failed " ++ show v ++ " =/= " ++ show t)
    else t' `matchesFull'` s'
matchesFull' t (Choice s11 s12 s2) = either (\_msg -> t `matchesFull'` andThen s12 s2) Right (t `matchesFull'` andThen s11 s2)
matchesFull' t (CondChoice p s11 s12 s2) = if predVal p then t `matchesFull'` andThen s12 s2 else t `matchesFull'` andThen s11 s2
matchesFull' t (TillT s s') = t `matchesFull'` andThen s (JumpPoint s s')
matchesFull' t (InternalT (JumpPoint _ s')) = t `matchesFull'` s'
matchesFull' t (JumpPoint s s') = t `matchesFull'` andThen s (JumpPoint s s')
matchesFull' t s = Left $ "Got stuck with trace " ++ show t ++ "\nAnd spec " ++ show s

type MatchM = WriterT [Int] IO (Either String ())

-- matching against partial traces with on demand extension
matchesPartial :: Gen Int -> PartialTrace String () -> Spec VarName -> MatchM
matchesPartial gen partial spec = matchesPartial' partial (unsugarT spec)
  where
  matchesPartial' :: PartialTrace String () -> Spec VarName -> MatchM
  matchesPartial' (Finish' ()) Nop = return $ Right ()
  matchesPartial' (Pure frag) s = do
    t <- lift $ generate gen
    let trace = stepTrace (show t) frag
    tell [t] -- record generated value for feedback
    matchesPartial' trace s
  matchesPartial' (ProgRead' v t') (Read xs s') =
    let s'' = instantiateEither sigma s'
        sigma (Left ()) = Lit (read v)
        sigma (Right ys) = if xs == ys then Cons (Lit $ read v) (V xs) else V ys
    in t' `matchesPartial'` s''
  matchesPartial' (ProgWrite' v t') (Write t s') =
    if read v /= termVal t
      then return $ Left ("Equaltiy test failed " ++ v ++ " =/= " ++ show t ++ " == " ++ show (termVal t))
      else t' `matchesPartial'` s'
  matchesPartial' t (Choice s11 s12 s2) = (t `matchesPartial'` andThen s11 s2) >>= either (\_msg -> t `matchesPartial'` andThen s12 s2) (return . Right)
  matchesPartial' t (CondChoice p s11 s12 s2) = if predVal p then t `matchesPartial'` andThen s12 s2 else t `matchesPartial'` andThen s11 s2
  matchesPartial' t (TillT s s') = t `matchesPartial'` andThen s (JumpPoint s s')
  matchesPartial' t (InternalT (JumpPoint _ s')) = t `matchesPartial'` s'
  matchesPartial' t (JumpPoint s s') = t `matchesPartial'` andThen s (JumpPoint s s')
  matchesPartial' t s = return $ Left $ "Got stuck with trace " ++ show t ++ "\nAnd spec " ++ show s

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
