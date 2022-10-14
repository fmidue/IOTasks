{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
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
import Data.Kind

data Term a where
  Add :: Term Integer -> Term Integer -> Term Integer
  Sub :: Term Integer -> Term Integer -> Term Integer
  Mul :: Term Integer -> Term Integer -> Term Integer
  Equals :: Term Integer -> Term Integer -> Term Bool
  Gt :: Term Integer -> Term Integer -> Term Bool
  Ge :: Term Integer -> Term Integer -> Term Bool
  Lt :: Term Integer -> Term Integer -> Term Bool
  Le :: Term Integer -> Term Integer -> Term Bool
  And :: Term Bool -> Term Bool -> Term Bool
  Or :: Term Bool -> Term Bool -> Term Bool
  IsInT :: Term Integer -> Term [Integer] -> Term Bool
  NotT :: Term Bool -> Term Bool
  SumT :: Term [Integer] -> Term Integer
  ProductT :: Term [Integer] -> Term Integer
  LengthT :: Term [Integer] -> Term Integer
  IntLitT :: I -> Term Integer
  ListLitT :: [I] -> Term [Integer]
  BoolLitT :: Bool -> Term Bool
  Current :: VarExp a => a -> Int -> Term Integer
  All :: VarExp a => a -> Int -> Term [Integer]

termStruct :: Term a -> TermStruct a
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
termStruct (IntLitT x) = Literal $ IntLit x
termStruct (ListLitT x) = Literal $ ListLit x
termStruct (BoolLitT x) = Literal $ BoolLit x
termStruct (Current x n) = Var C x n
termStruct (All x n) = Var A x n

data TermStruct a where
  Unary :: (Typeable a, Typeable b) => UnaryF a b -> Term a -> TermStruct b
  Binary :: (Typeable a, Typeable b, Typeable c) => BinaryF a b c -> Term a -> Term b -> TermStruct c
  Literal :: Typeable a => ConstValue a -> TermStruct a
  Var :: (Typeable a, VarExp e) => AccessType a ->  e -> Int -> TermStruct a

data AccessType a where
  C :: AccessType Integer
  A :: AccessType [Integer]

data UnaryF a b where
  Not :: UnaryF Bool Bool
  Length :: UnaryF [Integer] Integer
  Sum :: UnaryF [Integer] Integer
  Product :: UnaryF [Integer] Integer

data BinaryF a b c where
  (:+:) :: BinaryF Integer Integer Integer
  (:-:) :: BinaryF Integer Integer Integer
  (:*:) :: BinaryF Integer Integer Integer
  (:==:) :: BinaryF Integer Integer Bool
  (:>:) :: BinaryF Integer Integer Bool
  (:>=:) :: BinaryF Integer Integer Bool
  (:<:) :: BinaryF Integer Integer Bool
  (:<=:) :: BinaryF Integer Integer Bool
  (:&&:) :: BinaryF Bool Bool Bool
  (:||:) :: BinaryF Bool Bool Bool
  IsIn :: BinaryF Integer [Integer] Bool

data ConstValue a where
  BoolLit :: Bool -> ConstValue Bool
  IntLit :: I -> ConstValue Integer
  ListLit :: [I] -> ConstValue [Integer]

-- deriving instance Eq (Term a)
-- deriving instance Ord (Term a)
-- deriving instance Show (Term a)

varExps :: Term a -> [[Varname]]
varExps (termStruct -> Binary _ x y) = varExps x ++ varExps y
varExps (termStruct -> Unary _ x) = varExps x
varExps (termStruct -> Literal _) = []
varExps (termStruct -> Var _ e _) = [toVarList e]

instance Accessor Term where
  currentValue' = Current
  allValues' = All

instance Arithmetic Term where
  (.+.) = Add
  (.-.) = Sub
  (.*.) = Mul
  intLit = IntLitT . fromInteger

instance Compare Term where
  (.==.) = Equals
  (.>.) = Gt
  (.>=.) = Ge
  (.<.) = Lt
  (.<=.) = Le

instance Logic Term where
  not' = NotT
  x .&&. (BoolLitT True) = x
  BoolLitT True .&&. y  = y
  x .&&. y = And x y
  x .||. BoolLitT False = x
  BoolLitT False .||. y  = y
  x .||. y = Or x y
  true = BoolLitT True
  false = BoolLitT False

instance Sets Term where
  isIn = IsInT

instance BasicLists Term where
  length' = LengthT
  sum' = SumT
  product' = ProductT
  listLit = ListLitT . map fromInteger

instance TypeError (Text "complex list functions, like filter, can not be used at type " :<>: ShowType Term)
  => ComplexLists Term where
  filter' = error "unreachable"

-- Overflow detection type
class Typeable a => OverflowType a where
  type OT a :: Type
  typeRepT :: TypeRep (OT a)

instance OverflowType Integer where
  type OT Integer = I
  typeRepT = typeRep

instance OverflowType Bool where
  type OT Bool = Bool
  typeRepT = typeRep

instance OverflowType a => OverflowType [a] where
  type OT [a] = [OT a]
  typeRepT = withTypeable (typeRepT @a) typeRep

eval :: forall a. OverflowType a => Term a -> Map Varname [(Integer,Int)] -> (OverflowWarning, a)
eval t m =
  let r = eval' t m
  in case eqTypeRep (withTypeable (typeRepT @a) (typeOf r)) (typeRep @(OverflowWarning,a)) of
    Just HRefl -> r
    Nothing -> case eqTypeRep (typeRep @a) (typeRep @Integer) of
      Just HRefl -> second toInteger r
      Nothing -> case eqTypeRep (typeRep @a) (typeRep @[Integer]) of
        Just HRefl -> second (map toInteger) r
        Nothing -> error "eval: impossible"

evalF :: UnaryF a b -> OT a -> OT b
evalF Not = not
evalF Length = fromIntegral . length
evalF Sum = sum
evalF Product = product

evalF2 :: BinaryF a b c -> OT a -> OT b -> OT c
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

eval' :: Term a -> Map Varname [(Integer,Int)] -> (OverflowWarning,OT a)
eval' (termStruct -> Binary (f :: BinaryF a b c) x y) e =
  case eqTypeRep (typeRep @c) (typeRep @Integer) of
    Just HRefl -> let (w,r) = evalF2 f <$> eval' x e <*> eval' y e in (checkOverflow r <> w, r)
    Nothing -> evalF2 f <$> eval' x e <*> eval' y e
eval' (termStruct -> Unary f b) e = evalF f <$> eval' b e
eval' (termStruct -> Literal (BoolLit b)) _ = (mempty,b)
eval' (termStruct -> Literal (IntLit n)) _ = (checkOverflow n ,n)
eval' (termStruct -> Literal (ListLit xs)) _ = let xs' = xs in (foldMap checkOverflow xs', xs')
eval' (termStruct -> Var C x n) e = fromMaybe (error $ "empty list for {" ++ intercalate "," (toVarList x) ++ "}") . safeHead <$> primEvalVar x n e
eval' (termStruct -> Var A x n) e = reverse <$> primEvalVar x n e

primEvalVar :: VarExp a => a -> Int -> Map Varname [(Integer,Int)] -> (OverflowWarning,[I])
primEvalVar x n e =
  let xs = drop n . (map (fromInteger . fst) . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) (toVarList x)
  in (mconcatMap checkOverflow xs,xs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

printIndexedTerm :: Term a -> Map Varname (Int,[Int]) -> String
printIndexedTerm (termStruct -> Binary IsIn x xs) m = printIndexedTerm x m ++ " ∈ " ++ printIndexedTerm xs m
printIndexedTerm (termStruct -> Binary f tx ty) m = concat ["(",printIndexedTerm tx m, ") ",fSym f," (", printIndexedTerm ty m,")"]
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
printIndexedTerm (termStruct -> Unary Not (IsInT x xs)) m = printIndexedTerm x m ++ " ∉ " ++ printIndexedTerm xs m
printIndexedTerm (termStruct -> Unary Not t) m = concat ["not (", printIndexedTerm t m, ")"]
printIndexedTerm (termStruct -> Literal (BoolLit True)) _ = "True"
printIndexedTerm (termStruct -> Literal (BoolLit False)) _ = "False"
printIndexedTerm (termStruct -> Unary Length t) m = concat ["length (", printIndexedTerm t m, ")"]
printIndexedTerm (termStruct -> Unary Sum t) m = concat ["sum (", printIndexedTerm t m, ")"]
printIndexedTerm (termStruct -> Unary Product t) m = concat ["product (", printIndexedTerm t m, ")"]
printIndexedTerm (termStruct -> Var C x n) m = (\(x,(i,_)) -> x ++ "_" ++ show i) $ maximumOn (head.snd.snd) $ (\xs -> take (length xs - n) xs) $ mapMaybe (\x -> (x,) <$> Map.lookup x m) (toVarList x)
printIndexedTerm (termStruct -> Var A x n) _ = "{" ++ intercalate "," (toVarList x) ++ "}"++":"++show n++"_A"
printIndexedTerm (termStruct -> Literal (IntLit x)) _ = show x
printIndexedTerm (termStruct -> Literal (ListLit xs)) _ = show xs

data SomeTerm where
  SomeTerm :: OverflowType a => Term a -> SomeTerm

someTerm :: Term a -> SomeTerm
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
someTerm t@(IntLitT _) = SomeTerm t
someTerm t@(ListLitT _) = SomeTerm t
someTerm t@(BoolLitT _) = SomeTerm t
someTerm t@(Current _ _) = SomeTerm t
someTerm t@(All _ _) = SomeTerm t

withO :: SomeTerm -> (forall a. OverflowType a => Term a -> r) -> r
withO (SomeTerm t) f = f t

subTerms :: Term a -> [SomeTerm]
subTerms (termStruct' -> (t,Unary _ (x :: Term a))) = someTerm t : withO (someTerm x) subTerms
subTerms (termStruct' -> (t,Binary _ x y)) = someTerm t : subTerms x ++ subTerms y
subTerms (termStruct' -> (t,Literal{})) = [someTerm t]
subTerms (termStruct' -> (t,Var{})) = [someTerm t]

castTerm :: forall a. Typeable a => SomeTerm -> Maybe (Term a)
castTerm (SomeTerm (t :: Term b)) =
  case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> Just t
    Nothing -> Nothing

-- dirty hack
termStruct' :: Term a -> (Term a, TermStruct a)
termStruct' t = (t, termStruct t)
