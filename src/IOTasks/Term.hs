{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module IOTasks.Term where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)

type Varname = String

data Term a where
  (:+:) :: Term Integer -> Term Integer -> Term Integer
  (:-:) :: Term Integer -> Term Integer -> Term Integer
  (:*:) :: Term Integer -> Term Integer -> Term Integer
  (:==:) :: Term Integer -> Term Integer -> Term Bool
  (:>:) :: Term Integer -> Term Integer -> Term Bool
  (:>=:) :: Term Integer -> Term Integer -> Term Bool
  (:<:) :: Term Integer -> Term Integer -> Term Bool
  (:<=:) :: Term Integer -> Term Integer -> Term Bool
  Not :: Term Bool -> Term Bool
  (:&&:) :: Term Bool -> Term Bool -> Term Bool
  (:||:) :: Term Bool -> Term Bool -> Term Bool
  Length :: Term [Integer] -> Term Integer
  Sum :: Term [Integer] -> Term Integer
  Product :: Term [Integer] -> Term Integer
  Current :: Varname -> Term Integer
  All :: Varname -> Term [Integer]
  IntLit :: Integer -> Term Integer

deriving instance Eq (Term a)
deriving instance Ord (Term a)
deriving instance Show (Term a)

eval :: Term a -> Map Varname [Integer] -> a
eval (x :+: y) e = eval x e + eval y e
eval (x :-: y) e = eval x e - eval y e
eval (x :*: y) e = eval x e * eval y e
eval (x :==: y) e = eval x e == eval y e
eval (x :>: y) e = eval x e > eval y e
eval (x :>=: y) e = eval x e >= eval y e
eval (x :<=: y) e = eval x e <= eval y e
eval (x :<: y) e = eval x e < eval y e
eval (Not b) e = not $ eval b e
eval (x :&&: y) e = eval x e && eval y e
eval (x :||: y) e = eval x e || eval y e
eval (Length xs) e = fromIntegral . length $ eval xs e
eval (Sum xs) e = fromIntegral . sum $ eval xs e
eval (Product xs) e = fromIntegral . product $ eval xs e
eval (Current x) e = fromMaybe (error $ "empty list for " ++ x) $ safeHead $ eval (All x) e
eval (All x) e = fromMaybe [] $ Map.lookup x e
eval (IntLit n) _ = n

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: Term a -> Map Varname Int -> String
printIndexedTerm (tx :+: ty) m = concat ["(",printIndexedTerm tx m, ") + (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :-: ty) m = concat ["(",printIndexedTerm tx m, ") - (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :*: ty) m = concat ["(",printIndexedTerm tx m, ") * (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :==: ty) m = concat ["(",printIndexedTerm tx m, ") == (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>: ty) m = concat ["(",printIndexedTerm tx m, ") > (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>=: ty) m = concat ["(",printIndexedTerm tx m, ") >= (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<: ty) m = concat ["(",printIndexedTerm tx m, ") < (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<=: ty) m = concat ["(",printIndexedTerm tx m, ") <= (", printIndexedTerm ty m,")"]
printIndexedTerm (Not t) m = concat ["not (", printIndexedTerm t m, ")"]
printIndexedTerm (tx :&&: ty) m = concat ["(",printIndexedTerm tx m, ") && (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :||: ty) m = concat ["(",printIndexedTerm tx m, ") || (", printIndexedTerm ty m,")"]
printIndexedTerm (Length t) m = concat ["length (", printIndexedTerm t m, ")"]
printIndexedTerm (Sum t) m = concat ["sum (", printIndexedTerm t m, ")"]
printIndexedTerm (Product t) m = concat ["product (", printIndexedTerm t m, ")"]
printIndexedTerm (Current x) m = concat [x,"_",show $ fromMaybe 0 $ Map.lookup x m]
printIndexedTerm (All x) _ = x++"_A"
printIndexedTerm (IntLit x) _ = show x
