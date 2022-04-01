{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Term where
import Data.Maybe (fromMaybe)

type Varname = String

data Term a where
  (:+:) :: Term Integer -> Term Integer -> Term Integer
  (:-:) :: Term Integer -> Term Integer -> Term Integer
  (:*:) :: Term Integer -> Term Integer -> Term Integer
  (:==:) :: Term Integer -> Term Integer -> Term Bool
  (:>:) :: Term Integer -> Term Integer -> Term Bool
  Not :: Term Bool -> Term Bool
  Length :: Term [Integer] -> Term Integer
  Sum :: Term [Integer] -> Term Integer
  Current :: Varname -> Term Integer
  All :: Varname -> Term [Integer]

eval :: Term a -> [(Varname,[Integer])] -> a
eval (x :+: y) e = eval x e + eval y e
eval (x :-: y) e = eval x e - eval y e
eval (x :*: y) e = eval x e * eval y e
eval (x :==: y) e = eval x e == eval y e
eval (x :>: y) e = eval x e > eval y e
eval (Not b) e = not $ eval b e
eval (Length xs) e = fromIntegral . length $ eval xs e
eval (Sum xs) e = fromIntegral . sum $ eval xs e
eval (Current x) e = fromMaybe (error $ "empty list for " ++ x) $ safeHead $ eval (All x) e
eval (All x) e = fromMaybe (error $ "no value for " ++ x) $ lookup x e

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
