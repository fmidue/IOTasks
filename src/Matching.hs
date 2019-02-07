module Matching where

import Type
import IOtt (Trace'(..))

import Bound.Scope
import Data.Either
import Data.Maybe

matches :: Trace' Int () -> Spec VarName -> Either String ()
matches t s = matches' t (unsugarT s)

matches' :: Trace' Int () -> Spec VarName -> Either String ()
matches' (Finish ()) Nop = Right ()
matches' (ProgRead v t') (Read xs s') =
  let s'' = instantiateEither (f v xs) s'
  in t' `matches'` s''
matches' (ProgWrite v t') (Write t s') =
  if v /= termVal t
    then Left ("Equaltiy test failed " ++ show v ++ " =/= " ++ show t)
    else t' `matches'` s'
matches' t (Choice s11 s12 s2) = either (\_msg -> t `matches'` andThen s12 s2) Right (t `matches'` andThen s11 s2)
matches' t (CondChoice p s11 s12 s2) = if predVal p then t `matches'` andThen s12 s2 else t `matches'` andThen s11 s2
matches' t (TillT s s') = t `matches'` andThen s (JumpPoint s s')
matches' t (InternalT (JumpPoint _ s')) = t `matches'` s'
matches' t (JumpPoint s s') = t `matches'` andThen s (JumpPoint s s')
matches' t s = Left $ "Got stuck with trace " ++ show t ++ "\nAnd spec " ++ show s

-- TODO rename and merge into matches function
f :: Int -> VarName -> Either () VarName -> Term VarName
f v _ (Left ()) = Lit v
f v xs (Right ys) = if xs == ys then Cons (Lit v) (V xs) else V ys

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
