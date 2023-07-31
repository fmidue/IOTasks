{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Test.IOTasks.Internal.ConditionTerm (
  ConditionTerm(..),
  eval,
  termVarExps, subTerms,
  printTerm, printIndexedTerm,
  SomeTerm(..),
  castTerm,
  TermStruct(..),
  termStruct,
  UnaryF(..), BinaryF(..), ConstValue(..)
  ) where

import Test.IOTasks.Terms
import Test.IOTasks.Internal.Overflow
import Test.IOTasks.ValueMap as ValueMap

import Data.Maybe (fromMaybe,mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.List (sortBy, intercalate)
import Data.Function (on)
import Data.List.Extra (maximumOn, mconcatMap)
import Data.Bifunctor (second)

import Type.Reflection
import GHC.TypeLits
import Type.Match (matchType, fallbackCase', inCaseOfE', matchTypeOf, inCaseOfE)

data ConditionTerm a where
  Add :: ConditionTerm Integer -> ConditionTerm Integer -> ConditionTerm Integer
  Sub :: ConditionTerm Integer -> ConditionTerm Integer -> ConditionTerm Integer
  Mul :: ConditionTerm Integer -> ConditionTerm Integer -> ConditionTerm Integer
  Equals :: (OverflowType a, Eq a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Gt :: (OverflowType a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Ge :: (OverflowType a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Lt :: (OverflowType a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Le :: (OverflowType a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  And :: ConditionTerm Bool -> ConditionTerm Bool -> ConditionTerm Bool
  Or :: ConditionTerm Bool -> ConditionTerm Bool -> ConditionTerm Bool
  IsInT :: ConditionTerm Integer -> ConditionTerm [Integer] -> ConditionTerm Bool
  NotT :: ConditionTerm Bool -> ConditionTerm Bool
  SumT :: ConditionTerm [Integer] -> ConditionTerm Integer
  ProductT :: ConditionTerm [Integer] -> ConditionTerm Integer
  LengthT :: Typeable a => ConditionTerm [a] -> ConditionTerm Integer
  ReverseT :: OverflowType a => ConditionTerm [a] -> ConditionTerm [a]
  IntLitT :: I -> ConditionTerm Integer
  ListLitT :: OverflowType a => [OT a] -> ConditionTerm [a]
  BoolLitT :: Bool -> ConditionTerm Bool
  Current :: (OverflowType a, VarExp e) => e -> Int -> ConditionTerm a
  All :: (OverflowType a, VarExp e) => e -> Int -> ConditionTerm [a]

termStruct :: ConditionTerm a -> TermStruct a
termStruct (Add x y) = Binary (:+:) x y
termStruct (Sub x y) = Binary (:-:) x y
termStruct (Mul x y) = Binary (:*:) x y
termStruct (Equals x y) = Binary (:==:) x y
termStruct (Lt x y) = Binary (:<:) x y
termStruct (Le x y) = Binary (:<=:) x y
termStruct (Gt x y) = Binary (:>:) x y
termStruct (Ge x y) = Binary (:>=:) x y
termStruct (And x y) = Binary (:&&:) x y
termStruct (Or x y) = Binary (:||:) x y
termStruct (IsInT x y) = Binary IsIn x y
termStruct (NotT x) = Unary Not x
termStruct (SumT x) = Unary Sum x
termStruct (ProductT x) = Unary Product x
termStruct (LengthT x) = Unary Length x
termStruct (ReverseT x) = Unary Reverse x
termStruct (IntLitT x) = Literal $ IntLit x
termStruct (ListLitT x) = Literal $ ListLit x
termStruct (BoolLitT x) = Literal $ BoolLit x
termStruct (Current x n) = VariableC x n
termStruct (All x n) = VariableA x n

data TermStruct a where
  Unary :: (Typeable a, Typeable b) => UnaryF a b -> ConditionTerm a -> TermStruct b
  Binary :: (OverflowType a, OverflowType b, OverflowType c) => BinaryF a b c -> ConditionTerm a -> ConditionTerm b -> TermStruct c
  Literal :: Typeable a => ConstValue a -> TermStruct a
  VariableC :: (OverflowType a, VarExp e) => e -> Int -> TermStruct a
  VariableA :: (OverflowType a, VarExp e) => e -> Int -> TermStruct [a]

data UnaryF a b where
  Not :: UnaryF Bool Bool
  Length :: Typeable a => UnaryF [a] Integer
  Reverse :: Typeable a => UnaryF [a] [a]
  Sum :: UnaryF [Integer] Integer
  Product :: UnaryF [Integer] Integer

data BinaryF a b c where
  (:+:) :: BinaryF Integer Integer Integer
  (:-:) :: BinaryF Integer Integer Integer
  (:*:) :: BinaryF Integer Integer Integer
  (:==:) :: (Typeable a, Eq a) => BinaryF a a Bool
  (:>:) :: (OverflowType a, Ord a) => BinaryF a a Bool
  (:>=:) :: (OverflowType a, Ord a) => BinaryF a a Bool
  (:<:) :: (OverflowType a, Ord a) => BinaryF a a Bool
  (:<=:) :: (OverflowType a, Ord a) => BinaryF a a Bool
  (:&&:) :: BinaryF Bool Bool Bool
  (:||:) :: BinaryF Bool Bool Bool
  IsIn :: BinaryF Integer [Integer] Bool

data ConstValue a where
  BoolLit :: Bool -> ConstValue Bool
  IntLit :: I -> ConstValue Integer
  ListLit :: OverflowType a => [OT a] -> ConstValue [a]

termVarExps :: ConditionTerm a -> [[SomeVar]]
termVarExps (termStruct -> Binary _ x y) = termVarExps x ++ termVarExps y
termVarExps (termStruct -> Unary _ x) = termVarExps x
termVarExps (termStruct -> Literal _) = []
termVarExps (termStruct -> VariableC e _) = [toVarList e]
termVarExps (termStruct -> VariableA e _) = [toVarList e]

instance Accessor ConditionTerm where
  recentValue :: forall a e. (Typeable a, VarExp e) => e -> Int -> ConditionTerm a
  recentValue =  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Current
    , inCaseOfE' @String $ \HRefl -> Current
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]
  recentValues :: forall a e. (Typeable a, VarExp e) => e -> Int -> ConditionTerm [a]
  recentValues =  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> All
    , inCaseOfE' @String $ \HRefl -> All
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]

instance Arithmetic ConditionTerm where
  (.+.) = Add
  (.-.) = Sub
  (.*.) = Mul
  intLit = IntLitT . fromInteger

instance Compare ConditionTerm where
  (.==.) = Equals
  (.>.) = Gt
  (.>=.) = Ge
  (.<.) = Lt
  (.<=.) = Le

instance Logic ConditionTerm where
  not' = NotT
  x .&&. (BoolLitT True) = x
  BoolLitT True .&&. y  = y
  x .&&. y = And x y
  x .||. BoolLitT False = x
  BoolLitT False .||. y  = y
  x .||. y = Or x y
  true = BoolLitT True
  false = BoolLitT False

instance Sets ConditionTerm where
  isIn = IsInT

instance BasicLists ConditionTerm where
  length' = LengthT
  reverse' = ReverseT
  sum' = SumT
  product' = ProductT
  listLit = ListLitT . toOT

instance TypeError (Text "complex list functions, like filter, can not be used at type " :<>: ShowType ConditionTerm)
  => ComplexLists ConditionTerm where
  filter' = error "unreachable"

eval :: forall a. OverflowType a => ConditionTerm a -> ValueMap -> (OverflowWarning, a)
eval t m =
  let r = eval' t m
  in matchTypeOf r
    [ inCaseOfE' @(OverflowWarning, a) $ \HRefl -> r
    , fallbackCase' $ matchType @a
        [ inCaseOfE' @Integer $ \HRefl -> second toInteger r
        , inCaseOfE' @[Integer] $ \HRefl -> second (map toInteger) r
        , inCaseOfE' @String $ \HRefl -> r
        , fallbackCase' $ error "eval: impossible"
        ]
    ]

evalF :: UnaryF a b -> OT a -> OT b
evalF Not = not
evalF Length = fromIntegral . length
evalF Reverse = reverse
evalF Sum = sum
evalF Product = product

evalF2 :: forall a b c. OverflowType a => BinaryF a b c -> OT a -> OT b -> OT c
evalF2 (:+:) = (+)
evalF2 (:-:) = (-)
evalF2 (:*:) = (*)
evalF2 (:==:) = (==)
evalF2 (:>:) = (>)
evalF2 (:>=:) = (>=)
evalF2 (:<:) = (<)
evalF2 (:<=:) = (<=)
evalF2 (:&&:) = (&&)
evalF2 (:||:) = (||)
evalF2 IsIn = elem

eval' :: forall x. ConditionTerm x -> ValueMap -> (OverflowWarning,OT x)
eval' (termStruct -> Binary (f :: BinaryF a b c) x y) e =
  matchType @c
    [ inCaseOfE' @Integer $ \HRefl -> let (w,r) = evalF2 f <$> eval' x e <*> eval' y e in (checkOverflow r <> w, r)
    , fallbackCase' (evalF2 f <$> eval' x e <*> eval' y e)
    ]
eval' (termStruct -> Unary (f :: UnaryF a b) x) e = matchType @b
  [ inCaseOfE' @Integer $ \HRefl -> let (w,r) = evalF f <$> eval' x e in (checkOverflow r <> w, r)
  , fallbackCase' (evalF f <$> eval' x e)
  ]
eval' (termStruct -> Literal (BoolLit b)) _ = (mempty,b)
eval' (termStruct -> Literal (IntLit n)) _ = (checkOverflow n ,n)
eval' (termStruct -> Literal (ListLit xs)) _ = let xs' = xs in (foldMap checkOverflow xs', xs')
eval' (termStruct -> VariableC x n) e = fromMaybe (error $ "empty list for {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}") . safeHead <$> primEvalVar x n e
eval' (termStruct -> (VariableA x n)) e = reverse <$> primEvalVar x n e

primEvalVar :: forall a e. (OverflowType a, VarExp e) => e -> Int -> ValueMap -> (OverflowWarning,[OT a])
primEvalVar x n e =
  let xs = drop n . map fst . sortBy (flip compare `on` snd) . concatMap unwrapValueEntry $ mapMaybe (`ValueMap.lookup` e) (toVarList x)
  in matchTypeOf xs
    [ inCaseOfE @[I] $ \HRefl xs -> (mconcatMap (checkOverflow @Integer) xs,xs)
    , fallbackCase' (mempty,xs)
    ]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: ConditionTerm a -> Map SomeVar (Int,[Int]) -> String
printIndexedTerm t = printTerm' t . Just

printTerm :: ConditionTerm a -> String
printTerm t = printTerm' t Nothing

printTerm' :: ConditionTerm a -> Maybe (Map SomeVar (Int,[Int])) -> String
printTerm' (termStruct -> Binary IsIn x xs) m = printTerm' x m ++ " ∈ " ++ printTerm' xs m
printTerm' (termStruct -> Binary f tx ty) m = concat ["(",printTerm' tx m, ") ",fSym f," (", printTerm' ty m,")"]
  where
    fSym :: BinaryF a b c -> String
    fSym (:+:) = "+"
    fSym (:-:) = "-"
    fSym (:*:) = "*"
    fSym (:==:) = "=="
    fSym (:>:) = ">"
    fSym (:>=:) = ">="
    fSym (:<:) = "<"
    fSym (:<=:) = "<="
    fSym (:&&:) = "&&"
    fSym (:||:) = "||"
    fSym IsIn = error "handled by special case above"
printTerm' (termStruct -> Unary Not (IsInT x xs)) m = printTerm' x m ++ " ∉ " ++ printTerm' xs m
printTerm' (termStruct -> Unary Not t) m = concat ["not (", printTerm' t m, ")"]
printTerm' (termStruct -> Literal (BoolLit True)) _ = "True"
printTerm' (termStruct -> Literal (BoolLit False)) _ = "False"
printTerm' (termStruct -> Unary f t) m = concat [fSym f ++" (", printTerm' t m, ")"]
  where
    fSym :: UnaryF a b -> String
    fSym Not = "not"
    fSym Length = "length"
    fSym Reverse = "reverse"
    fSym Sum = "sum"
    fSym Product = "product"
printTerm' (termStruct -> VariableC x n) (Just m) = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (someVarname x,) <$> Map.lookup x m) (toVarList x)
printTerm' (termStruct -> VariableC x n) Nothing = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_C"
printTerm' (termStruct -> VariableA x n) _ = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_A"
printTerm' (termStruct -> Literal (IntLit x)) _ = show x
printTerm' (termStruct -> Literal (ListLit xs)) _ = showOT xs

data SomeTerm where
  SomeTerm :: OverflowType a => ConditionTerm a -> SomeTerm

someTerm :: ConditionTerm a -> SomeTerm
someTerm t@(Add _ _) = SomeTerm t
someTerm t@(Sub _ _) = SomeTerm t
someTerm t@(Mul _ _) = SomeTerm t
someTerm t@(Equals _ _) = SomeTerm t
someTerm t@(Gt _ _) = SomeTerm t
someTerm t@(Ge _ _) = SomeTerm t
someTerm t@(Lt _ _) = SomeTerm t
someTerm t@(Le _ _) = SomeTerm t
someTerm t@(And _ _) = SomeTerm t
someTerm t@(Or _ _) = SomeTerm t
someTerm t@(IsInT _ _) = SomeTerm t
someTerm t@(NotT _) = SomeTerm t
someTerm t@(SumT _) = SomeTerm t
someTerm t@(ProductT _) = SomeTerm t
someTerm t@(LengthT _) = SomeTerm t
someTerm t@(ReverseT _) = SomeTerm t
someTerm t@(IntLitT _) = SomeTerm t
someTerm t@(ListLitT _) = SomeTerm t
someTerm t@(BoolLitT _) = SomeTerm t
someTerm t@(Current _ _) = SomeTerm t
someTerm t@(All _ _) = SomeTerm t

subTerms :: ConditionTerm a -> [SomeTerm]
subTerms (termStruct' -> (t,Unary _ x)) = someTerm t : subTerms x
subTerms (termStruct' -> (t,Binary _ x y)) = someTerm t : subTerms x ++ subTerms y
subTerms (termStruct' -> (t,Literal{})) = [someTerm t]
subTerms (termStruct' -> (t,VariableC{})) = [someTerm t]
subTerms (termStruct' -> (t,VariableA{})) = [someTerm t]

castTerm :: forall a. Typeable a => SomeTerm -> Maybe (ConditionTerm a)
castTerm (SomeTerm (t :: ConditionTerm b)) =
  matchType @a
    [ inCaseOfE' @b $ \HRefl -> Just t
    , fallbackCase' Nothing
    ]

-- dirty hack
termStruct' :: ConditionTerm a -> (ConditionTerm a, TermStruct a)
termStruct' t = (t, termStruct t)
