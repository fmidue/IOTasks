{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE TupleSections #-}
module Test.IOTasks.Internal.ConditionTerm (
  ConditionTerm(..),
  oEval,
  evalI, evalIs,
  termVarExps, subTerms,
  printTerm, printIndexedTerm,
  SomeTerm(..),
  castTerm,
  ) where

import Test.IOTasks.Terms
import Test.IOTasks.Internal.Overflow
import Test.IOTasks.ValueMap as ValueMap

import Data.Maybe (fromMaybe,mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.List (sortBy, intercalate)
import Data.Function (on)
import Data.List.Extra (maximumOn)

import Type.Reflection
import GHC.TypeLits
import Type.Match (matchType, fallbackCase', inCaseOfE')
import Control.Applicative

data ConditionTerm a where
  Add :: Num a => ConditionTerm a -> ConditionTerm a -> ConditionTerm a
  Sub :: Num a => ConditionTerm a -> ConditionTerm a -> ConditionTerm a
  Mul :: Num a => ConditionTerm a -> ConditionTerm a -> ConditionTerm a
  Equals :: (Typeable a, Eq a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Gt :: (Typeable a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Ge :: (Typeable a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Lt :: (Typeable a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  Le :: (Typeable a, Ord a) => ConditionTerm a -> ConditionTerm a -> ConditionTerm Bool
  And :: ConditionTerm Bool -> ConditionTerm Bool -> ConditionTerm Bool
  Or :: ConditionTerm Bool -> ConditionTerm Bool -> ConditionTerm Bool
  IsIn :: ConditionTerm Integer -> ConditionTerm [Integer] -> ConditionTerm Bool
  Not :: ConditionTerm Bool -> ConditionTerm Bool
  Sum :: Num a => ConditionTerm [a] -> ConditionTerm a
  Product :: Num a => ConditionTerm [a] -> ConditionTerm a
  Length :: Typeable a => ConditionTerm [a] -> ConditionTerm Integer
  Reverse :: Typeable a => ConditionTerm [a] -> ConditionTerm [a]
  IntLit :: Integer -> ConditionTerm Integer
  ListLit :: (Show a, Typeable a) => [a] -> ConditionTerm [a]
  BoolLit :: Bool -> ConditionTerm Bool
  Current :: VarExp e => e -> Int -> ConditionTerm a
  All :: (Typeable a, VarExp e) => e -> Int -> ConditionTerm [a]

termVarExps :: Typeable a => ConditionTerm a -> [[SomeVar]]
termVarExps (Add x y) = termVarExps x ++ termVarExps y
termVarExps (Sub x y) = termVarExps x ++ termVarExps y
termVarExps (Mul x y) = termVarExps x ++ termVarExps y
termVarExps (Equals x y) = termVarExps x ++ termVarExps y
termVarExps (Gt x y) = termVarExps x ++ termVarExps y
termVarExps (Ge x y) = termVarExps x ++ termVarExps y
termVarExps (Lt x y) = termVarExps x ++ termVarExps y
termVarExps (Le x y) = termVarExps x ++ termVarExps y
termVarExps (And x y) = termVarExps x ++ termVarExps y
termVarExps (Or x y) = termVarExps x ++ termVarExps y
termVarExps (IsIn x y) = termVarExps x ++ termVarExps y
termVarExps (Sum x) = termVarExps x
termVarExps (Product x) = termVarExps x
termVarExps (Length x) = termVarExps x
termVarExps (Reverse x) = termVarExps x
termVarExps (Not x) = termVarExps x
termVarExps (IntLit _) = []
termVarExps (ListLit _) = []
termVarExps (BoolLit _) = []
termVarExps (Current e _) = [toVarList e]
termVarExps (All e _) = [toVarList e]

instance Accessor ConditionTerm where
  valueBefore :: forall a e. (Typeable a, VarExp e) => Int -> e -> ConditionTerm a
  valueBefore =  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> flip Current
    , inCaseOfE' @String $ \HRefl -> flip Current
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]
  valuesBefore :: forall a e. (Typeable a, VarExp e) => Int -> e -> ConditionTerm [a]
  valuesBefore =  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> flip All
    , inCaseOfE' @String $ \HRefl -> flip All
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]

instance Arithmetic ConditionTerm where
  (.+.) = Add
  (.-.) = Sub
  (.*.) = Mul
  intLit = IntLit . fromInteger

instance Compare ConditionTerm where
  (.==.) = Equals
  x ./=. y = not' $ x .==. y
  (.>.) = Gt
  (.>=.) = Ge
  (.<.) = Lt
  (.<=.) = Le

instance Logic ConditionTerm where
  not' = Not
  x .&&. (BoolLit True) = x
  BoolLit True .&&. y  = y
  x .&&. y = And x y
  x .||. BoolLit False = x
  BoolLit False .||. y  = y
  x .||. y = Or x y
  true = BoolLit True
  false = BoolLit False

instance Membership ConditionTerm where
  isIn = IsIn

instance BasicLists ConditionTerm where
  length' = Length
  reverse' = Reverse
  sum' = Sum
  product' = Product
  listLit = ListLit

instance TypeError (Text "complex list functions, like filter, can not be used at type " :<>: ShowType ConditionTerm)
  => ComplexLists ConditionTerm where
  filter' = error "unreachable"

instance EffectEval ConditionTerm where
  type Env ConditionTerm = ValueMap
  pureEval f _ (Add x y) = liftA2 (+) (f x) (f y)
  pureEval f _ (Sub x y) = liftA2 (-) (f x) (f y)
  pureEval f _ (Mul x y) = liftA2 (*) (f x) (f y)
  pureEval f _ (Equals x y) = liftA2 (==) (f x) (f y)
  pureEval f _ (Gt x y) = liftA2 (>) (f x) (f y)
  pureEval f _ (Ge x y) = liftA2 (>=) (f x) (f y)
  pureEval f _ (Lt x y) = liftA2 (<) (f x) (f y)
  pureEval f _ (Le x y) = liftA2 (<=) (f x) (f y)
  pureEval f _ (And x y) = liftA2 (&&) (f x) (f y)
  pureEval f _ (Or x y) = liftA2 (||) (f x) (f y)
  pureEval f _ (IsIn x xs) = liftA2 elem (f x) (f xs)
  pureEval f _ (Not x) = not <$> f x
  pureEval f _ (Sum xs) = sum <$> f xs
  pureEval f _ (Product xs) = product <$> f xs
  pureEval f _ (Length xs) = toInteger . length <$> f xs
  pureEval f _ (Reverse xs) = reverse <$> f xs
  pureEval _ _ (IntLit x) = pure x
  pureEval _ _ (ListLit xs) = pure xs
  pureEval _ _ (BoolLit x) = pure x
  pureEval _ e (Current x n) = pure $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e
  pureEval _ e (All x n) = pure $ reverse $ primEvalVar x n e

oEval :: Typeable a => ValueMap -> ConditionTerm a -> (OverflowWarning, a)
oEval = evalOverflow (OverflowTreatment evalI (\d -> Right . evalIs d))

evalI :: ValueMap -> ConditionTerm Integer -> Either (SubCheck ConditionTerm I) I
evalI e (Add x y) = liftA2 (+) (evalI e x) (evalI e y)
evalI e (Sub x y) = liftA2 (-) (evalI e x) (evalI e y)
evalI e (Mul x y) = liftA2 (*) (evalI e x) (evalI e y)
evalI e (Sum xs) = Right $ sum $ evalIs e xs
evalI e (Product xs) = Right $ product $ evalIs e xs
evalI e (Length (xs :: ConditionTerm [a])) =
  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Right . fromInt . length $ evalIs e xs
    , fallbackCase' $ Left $ SubCheck xs (fromInt . length)
    ]
evalI _ (IntLit x) = Right $ fromInteger x
evalI e (Current x n) = Right $ fromInteger $ fromMaybe (error $ "empty list for {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}") . safeHead $ primEvalVar x n e

evalIs :: ValueMap -> ConditionTerm [Integer] -> [I]
evalIs d (Reverse xs) = reverse $ evalIs d xs
evalIs d (All v n) = map fromInteger $ primEvalVar v n d
evalIs _ (ListLit xs) = map fromInteger xs
evalIs _ Current{} = error "list variables are not supported"
evalIs _ Add{} = error "lists should not have a Num instance"
evalIs _ Sub{} = error "lists should not have a Num instance"
evalIs _ Mul{} = error "lists should not have a Num instance"
evalIs _ Sum{} = error "lists should not have a Num instance"
evalIs _ Product{} = error "lists should not have a Num instance"

primEvalVar :: forall a e. (Typeable a, VarExp e) => e -> Int -> ValueMap -> [a]
primEvalVar x n e =
  drop n . map fst . sortBy (flip compare `on` snd) . concatMap unwrapValueEntry $ mapMaybe (`ValueMap.lookup` e) (toVarList x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: Typeable a => ConditionTerm a -> Map SomeVar (Int,[Int]) -> String
printIndexedTerm t = printTerm' t . Just

printTerm :: Typeable a => ConditionTerm a -> String
printTerm t = printTerm' t Nothing

printTerm' :: Typeable a => ConditionTerm a -> Maybe (Map SomeVar (Int,[Int])) -> String
printTerm' (Add x y) m = printBinary "+" x y m
printTerm' (Sub x y) m = printBinary "-" x y m
printTerm' (Mul x y) m = printBinary "*" x y m
printTerm' (Equals x y) m = printBinary "==" x y m
printTerm' (Gt x y) m = printBinary ">" x y m
printTerm' (Ge x y) m = printBinary ">=" x y m
printTerm' (Lt x y) m = printBinary "<" x y m
printTerm' (Le x y) m = printBinary "<=" x y m
printTerm' (And x y) m = printBinary "&&" x y m
printTerm' (Or x y) m = printBinary "||" x y m
printTerm' (IsIn x xs) m = printTerm' x m ++ " ∈ " ++ printTerm' xs m
printTerm' (Not (IsIn x xs)) m = printTerm' x m ++ " ∉ " ++ printTerm' xs m
printTerm' (Not t) m = concat ["not (", printTerm' t m, ")"]
printTerm' (BoolLit b) _ = show b
printTerm' (Length xs) m = printUnary "length" xs m
printTerm' (Reverse xs) m = printUnary "reverse" xs m
printTerm' (Sum xs) m = printUnary "sum" xs m
printTerm' (Product xs) m = printUnary "product" xs m
printTerm' (Current x n) (Just m) = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (someVarname x,) <$> Map.lookup x m) (toVarList x)
printTerm' (Current x n) Nothing = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_C"
printTerm' (All x n) _ = "{" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"++":"++show n++"_A"
printTerm' (IntLit x) _ = show x
printTerm' (ListLit xs) _ = show xs

printBinary :: (Typeable a, Typeable b) => String -> ConditionTerm a -> ConditionTerm b -> Maybe (Map SomeVar (Int,[Int])) -> String
printBinary op x y m = concat ["(",printTerm' x m, ") ",op," (", printTerm' y m,")"]

printUnary :: Typeable a => String -> ConditionTerm a -> Maybe (Map SomeVar (Int,[Int])) -> String
printUnary op x m = concat [op ++" (", printTerm' x m, ")"]

data SomeTerm where
  SomeTerm :: Typeable a => ConditionTerm a -> SomeTerm
--
someTerm :: Typeable a => ConditionTerm a -> SomeTerm
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
someTerm t@(IsIn _ _) = SomeTerm t
someTerm t@(Not _) = SomeTerm t
someTerm t@(Sum _) = SomeTerm t
someTerm t@(Product _) = SomeTerm t
someTerm t@(Length _) = SomeTerm t
someTerm t@(Reverse _) = SomeTerm t
someTerm t@(IntLit _) = SomeTerm t
someTerm t@(ListLit _) = SomeTerm t
someTerm t@(BoolLit _) = SomeTerm t
someTerm t@(Current _ _) = SomeTerm t
someTerm t@(All _ _) = SomeTerm t
--
subTerms :: Typeable a => ConditionTerm a -> [SomeTerm]
subTerms t@(Add x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Sub x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Mul x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Equals x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Gt x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Ge x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Lt x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Le x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(And x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Or x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(IsIn x y) = someTerm t : subTerms x ++ subTerms y
subTerms t@(Sum x) = someTerm t : subTerms x
subTerms t@(Product x) = someTerm t : subTerms x
subTerms t@(Length x) = someTerm t : subTerms x
subTerms t@(Reverse x) = someTerm t : subTerms x
subTerms t@(Not x) = someTerm t : subTerms x
subTerms t@(IntLit _) = [someTerm t]
subTerms t@(ListLit _) = [someTerm t]
subTerms t@(BoolLit _) = [someTerm t]
subTerms t@(Current _ _) = [someTerm t]
subTerms t@(All _ _) = [someTerm t]

castTerm :: forall a. Typeable a => SomeTerm -> Maybe (ConditionTerm a)
castTerm (SomeTerm (t :: ConditionTerm b)) =
  matchType @a
    [ inCaseOfE' @b $ \HRefl -> Just t
    , fallbackCase' Nothing
    ]
