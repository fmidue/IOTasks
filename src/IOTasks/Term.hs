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
  Length :: Term [Integer] -> Term Integer
  Sum :: Term [Integer] -> Term Integer
  Product :: Term [Integer] -> Term Integer
  Current :: VarExp a => a -> Term Integer
  All :: VarExp a => a -> Term [Integer]
  IntLit :: Integer -> Term Integer

-- deriving instance Eq (Term a)
-- deriving instance Ord (Term a)
-- deriving instance Show (Term a)

instance Accessor Term where
  currentValue = Current
  allValues = All

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
  (.&&.) = (:&&:)
  (.||.) = (:||:)

instance BasicLists Term where
  length' = Length
  sum' = Sum
  product' = Product

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
eval (Length xs) e = fromIntegral . length $ eval xs e
eval (Sum xs) e = fromIntegral . sum $ eval xs e
eval (Product xs) e = fromIntegral . product $ eval xs e
eval (Current x) e = fromMaybe (error $ "empty list for {" ++ intercalate "," (toVarList x) ++ "}") $ safeHead $ eval (All x) e
eval (All x) e = (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) (toVarList x)
eval (IntLit n) _ = n

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
printIndexedTerm (Not t) m = concat ["not (", printIndexedTerm t m, ")"]
printIndexedTerm (tx :&&: ty) m = concat ["(",printIndexedTerm tx m, ") && (", printIndexedTerm ty m,")"]
printIndexedTerm (tx :||: ty) m = concat ["(",printIndexedTerm tx m, ") || (", printIndexedTerm ty m,")"]
printIndexedTerm (Length t) m = concat ["length (", printIndexedTerm t m, ")"]
printIndexedTerm (Sum t) m = concat ["sum (", printIndexedTerm t m, ")"]
printIndexedTerm (Product t) m = concat ["product (", printIndexedTerm t m, ")"]
printIndexedTerm (Current x) m = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ mapMaybe (\x -> (x,) <$> Map.lookup x m) (toVarList x)
printIndexedTerm (All x) _ = "{" ++ intercalate "," (toVarList x) ++ "}_A"
printIndexedTerm (IntLit x) _ = show x
