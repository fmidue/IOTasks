{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module IOTasks.Term where

import IOTasks.Terms

import Data.Maybe (fromMaybe,mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.List (sortBy, intercalate)
import Data.Function (on)
import Data.List.Extra (maximumOn)

import GHC.TypeLits

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
  TrueT :: Term Bool
  FalseT :: Term Bool
  Length :: Term [Integer] -> Term Integer
  Sum :: Term [Integer] -> Term Integer
  Product :: Term [Integer] -> Term Integer
  IsIn :: Term Integer -> Term [Integer] -> Term Bool
  Current :: VarExp a => a -> Int -> Term Integer
  All :: VarExp a => a -> Int -> Term [Integer]
  IntLit :: Integer -> Term Integer
  ListLit :: [Integer] -> Term [Integer]


-- deriving instance Eq (Term a)
-- deriving instance Ord (Term a)
-- deriving instance Show (Term a)

instance Accessor Term where
  currentValue' = Current
  allValues' = All

instance Arithmetic Term where
  (.+.) = (:+:)
  (.-.) = (:-:)
  (.*.) = (:*:)
  intLit = IntLit

instance Compare Term where
  (.==.) = (:==:)
  (.>.) = (:>:)
  (.>=.) = (:>=:)
  (.<.) = (:<:)
  (.<=.) = (:<=:)

instance Logic Term where
  not' = Not
  x .&&. TrueT = x
  TrueT .&&. y  = y
  x .&&. y = x :&&: y
  x .||. FalseT = x
  FalseT .||. y  = y
  x .||. y = x :||: y
  true = TrueT
  false = FalseT

instance Sets Term where
  isIn = IsIn

instance BasicLists Term where
  length' = Length
  sum' = Sum
  product' = Product
  listLit = ListLit

instance TypeError (Text "complex list functions, like filter, can not be used at type " :<>: ShowType Term)
  => ComplexLists Term where
  filter' = error "unreachable"

eval :: Term a -> Map Varname [(Integer,Int)] -> a
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
eval TrueT _ = True
eval FalseT _ = False
eval (Length xs) e = fromIntegral . length $ eval xs e
eval (Sum xs) e = fromIntegral . sum $ eval xs e
eval (Product xs) e = fromIntegral . product $ eval xs e
eval (Current x n) e = fromMaybe (error $ "empty list for {" ++ intercalate "," (toVarList x) ++ "}") $ safeHead $ primEvalVar x n e
eval (All x n) e = reverse $ primEvalVar x n e
eval (IntLit n) _ = n
eval (ListLit xs) _ = xs
eval (IsIn x xs) e = eval x e `elem` eval xs e

primEvalVar :: VarExp a => a -> Int -> Map Varname [(Integer,Int)] -> [Integer]
primEvalVar x n e = drop n . (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) (toVarList x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: Term a -> Map Varname (Int,[Int]) -> String
printIndexedTerm (tx :+: ty) m = concat ["(",printIndexedTerm tx m, ") + (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :-: ty) m = concat ["(",printIndexedTerm tx m, ") - (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :*: ty) m = concat ["(",printIndexedTerm tx m, ") * (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :==: ty) m = concat ["(",printIndexedTerm tx m, ") == (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>: ty) m = concat ["(",printIndexedTerm tx m, ") > (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :>=: ty) m = concat ["(",printIndexedTerm tx m, ") >= (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<: ty) m = concat ["(",printIndexedTerm tx m, ") < (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :<=: ty) m = concat ["(",printIndexedTerm tx m, ") <= (", printIndexedTerm ty m,")"]
printIndexedTerm (IsIn x xs) m = printIndexedTerm x m ++ " ∈ " ++ printIndexedTerm xs m
printIndexedTerm (Not (IsIn x xs)) m = printIndexedTerm x m ++ " ∉ " ++ printIndexedTerm xs m
printIndexedTerm (Not t) m = concat ["not (", printIndexedTerm t m, ")"]
printIndexedTerm (tx :&&: ty) m = concat ["(",printIndexedTerm tx m, ") && (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :||: ty) m = concat ["(",printIndexedTerm tx m, ") || (", printIndexedTerm ty m,")"]
printIndexedTerm TrueT _ = "True"
printIndexedTerm FalseT _ = "False"
printIndexedTerm (Length t) m = concat ["length (", printIndexedTerm t m, ")"]
printIndexedTerm (Sum t) m = concat ["sum (", printIndexedTerm t m, ")"]
printIndexedTerm (Product t) m = concat ["product (", printIndexedTerm t m, ")"]
printIndexedTerm (Current x n) m = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (x,) <$> Map.lookup x m) (toVarList x)
printIndexedTerm (All x n) _ = "{" ++ intercalate "," (toVarList x) ++ "}"++":"++show n++"_A"
printIndexedTerm (IntLit x) _ = show x
printIndexedTerm (ListLit xs) _ = show xs
