{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Term where
import Data.Maybe (fromMaybe)

type Varname = String

data Term a where
  (:+:) :: Term Int -> Term Int -> Term Int
  (:-:) :: Term Int -> Term Int -> Term Int
  (:*:) :: Term Int -> Term Int -> Term Int
  (:==:) :: Term Int -> Term Int -> Term Bool
  (:>:) :: Term Int -> Term Int -> Term Bool
  Not :: Term Bool -> Term Bool
  Length :: Term [Int] -> Term Int
  Current :: Varname -> Term Int
  All :: Varname -> Term [Int]

eval :: Term a -> [(Varname,[Int])] -> a
eval (x :+: y) e = eval x e + eval y e
eval (x :-: y) e = eval x e - eval y e
eval (x :*: y) e = eval x e * eval y e
eval (x :==: y) e = eval x e == eval y e
eval (x :>: y) e = eval x e > eval y e
eval (Not b) e = not $ eval b e
eval (Length xs) e = length $ eval xs e
eval (Current x) e = fromMaybe (error $ "empty list for" ++ x) $ safeHead $ eval (All x) e
eval (All x) e = fromMaybe (error $ "no value for " ++ x) $ lookup x e

safeHead :: [Int] -> Maybe Int
safeHead [] = Nothing
safeHead (x:_) = Just x
