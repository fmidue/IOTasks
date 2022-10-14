{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE FlexibleContexts #-}
module IOTasks.Term where

import IOTasks.Terms
import IOTasks.Overflow

import Data.Maybe (fromMaybe,mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.List (sortBy, intercalate)
import Data.Function (on)
import Data.List.Extra (maximumOn, mconcatMap)
import Data.Bifunctor (second)
import Type.Reflection

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
  IntLit :: I -> Term Integer
  ListLit :: [I] -> Term [Integer]


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
  intLit = IntLit . fromInteger

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
  listLit = ListLit . map fromInteger

instance TypeError (Text "complex list functions, like filter, can not be used at type " :<>: ShowType Term)
  => ComplexLists Term where
  filter' = error "unreachable"

-- Overflow detection type
type family OT a where
  OT Integer = I
  OT [a] = [OT a]
  OT a = a

eval :: forall a. (Typeable a, Typeable (OT a)) => Term a -> Map Varname [(Integer,Int)] -> (OverflowWarning, a)
eval t m =
  let r = eval' t m
  in case eqTypeRep (typeOf r) (typeRep @(OverflowWarning,a)) of
    Just HRefl -> r
    Nothing -> case eqTypeRep (typeRep @a) (typeRep @Integer) of
      Just HRefl -> second toInteger r
      Nothing -> error "impossible"

eval' :: Term a -> Map Varname [(Integer,Int)] -> (OverflowWarning,OT a)
eval' (x :+: y) e = let (w,r) = (+) <$> eval' x e <*> eval' y e in (checkOverflow r <> w, r)
eval' (x :-: y) e = let (w,r) = (-) <$> eval' x e <*> eval' y e in (checkOverflow r <> w, r)
eval' (x :*: y) e = let (w,r) = (*) <$> eval' x e <*> eval' y e in (checkOverflow r <> w, r)
eval' (x :==: y) e = (==) <$> eval' x e <*> eval' y e
eval' (x :>: y) e = (>) <$> eval' x e <*> eval' y e
eval' (x :>=: y) e = (>=) <$> eval' x e <*> eval' y e
eval' (x :<=: y) e = (<=) <$> eval' x e <*> eval' y e
eval' (x :<: y) e = (<) <$> eval' x e <*> eval' y e
eval' (Not b) e = not <$> eval' b e
eval' (x :&&: y) e = (&&) <$> eval' x e <*> eval' y e
eval' (x :||: y) e = (||) <$> eval' x e <*> eval' y e
eval' TrueT _ = (mempty,True)
eval' FalseT _ = (mempty,False)
eval' (Length xs) e = fromIntegral . length <$> eval' xs e
eval' (Sum xs) e = let (w,r) = sum <$> eval' xs e in (checkOverflow r <> w, r)
eval' (Product xs) e = let (w,r) = product <$> eval' xs e in (checkOverflow r <> w, r)
eval' (Current x n) e = fromMaybe (error $ "empty list for {" ++ intercalate "," (toVarList x) ++ "}") . safeHead <$> primEvalVar x n e
eval' (All x n) e = reverse <$> primEvalVar x n e
eval' (IntLit n) _ = (checkOverflow n ,n)
eval' (ListLit xs) _ = let xs' = xs in (foldMap checkOverflow xs', xs')
eval' (IsIn x xs) e = elem <$> eval' x e <*> eval' xs e

primEvalVar :: VarExp a => a -> Int -> Map Varname [(Integer,Int)] -> (OverflowWarning,[I])
primEvalVar x n e =
  let xs = drop n . (map (fromInteger . fst) . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) (toVarList x)
  in (mconcatMap checkOverflow xs,xs)

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
