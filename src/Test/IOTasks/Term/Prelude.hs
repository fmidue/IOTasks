{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Term.Prelude (
  -- * Accessors
  currentValue, allValues,
  valueBefore, valuesBefore,
  -- * Arithmetic functions
  (.+.), (.-.), (.*.),
  intLit,
  -- * Comparison functions
  (.==.), (./=.), (.>.), (.>=.), (.<.), (.<=.),
  -- * Boolean functions
  not',
  (.&&.), (.||.),
  true, false,
  -- * embedded values
  embeddedLit,
  -- * Simple list functions
  sum', product', length', reverse',
  isIn, isNotIn,
  listLit,
  -- * Complexer list functions
  filter',
  -- * Lifting of opaque functions
  liftOpaqueValue, liftOpaque, liftOpaque2,
  ) where

import Data.Char (isAlphaNum)
import Data.Express (Expr((:$)), value)

import Test.IOTasks.Internal.Term (Term(..), TermKind(..), toExpr, transparentSubterms, termVarExps)
import Test.IOTasks.Var (VarExp(..), Embedded, varname, Embeddable)

import Type.Match (matchType, inCaseOfE', inCaseOfApp', fallbackCase')
import Type.Reflection (Typeable, typeRep, (:~~:)(HRefl))

currentValue :: (Typeable a, VarExp e) => e a -> Term k a
-- ^ Defined as @'currentValue' = 'valueBefore' 0@, providing access to the current value.
currentValue = valueBefore 0

allValues :: (Typeable a, VarExp e) => e a -> Term k [a]
-- ^ Defined as @'allValues' = 'valuesBefore' 0@, providing access to all values.
allValues = valuesBefore 0

valueBefore :: (Typeable a, VarExp e) => Int -> e a -> Term k a
-- ^ If the variable-expression x is associated with the values [x_1,..,x_n],
-- @'valueBefore' i x@ provides access to x_(n-i).
valueBefore = valueBefore' where
  valueBefore' :: forall a e k. (Typeable a, VarExp e) => Int -> e a -> Term k a
  valueBefore' n x = checkNames x $ matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Current x n
    , inCaseOfE' @Bool $ \HRefl -> Current x n
    , inCaseOfE' @String $ \HRefl -> Current x n
    , inCaseOfApp' @Embedded $ \HRefl -> Current x n
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]
valuesBefore :: (Typeable a, VarExp e) => Int -> e a -> Term k [a]
-- ^ If the variable-expression x is associated with the values [x_1,..,x_n],
-- @'valuesBefore' i x@ provides access to [x_1,..,x_(n-i)].
valuesBefore = valuesBefore' where
  valuesBefore' :: forall a e k. (Typeable a, VarExp e) => Int -> e a -> Term k [a]
  valuesBefore' n x = checkNames x $ matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> All x n
    , inCaseOfE' @Bool $ \HRefl -> All x n
    , inCaseOfE' @String $ \HRefl -> All x n
    , inCaseOfApp' @Embedded $ \HRefl -> All x n
    , fallbackCase' $ error $ "variable type not supported for Terms: " ++ show (typeRep @a)
    ]

checkNames :: VarExp e => e a -> x -> x
checkNames = foldr (f . varname) id . toVarList
  where
    f x c = if legalVar x then c else error $ "illegal variable name: " ++ x ++ "\variable names must start with a letter and can only contain letters, digits, _ and '"

legalVar :: String -> Bool
legalVar [] = False
legalVar (x:xs) = isAlphaNum x && all (\c -> isAlphaNum c || c == '_' || c == '\'') xs

(.+.) :: Term k Integer -> Term k Integer -> Term k Integer
(.+.) = Add

(.-.) :: Term k Integer -> Term k Integer -> Term k Integer
(.-.) = Sub

(.*.) :: Term k Integer -> Term k Integer -> Term k Integer
(.*.) = Mul

intLit :: Integer -> Term k Integer
intLit = IntLit . fromInteger

(.==.) :: (Typeable a, Eq a) => Term k a -> Term k a -> Term k Bool
(.==.) = Equals

(./=.) :: (Typeable a, Eq a) => Term k a -> Term k a -> Term k Bool
x ./=. y = not' $ x .==. y

(.>.) :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
(.>.) = Gt

(.>=.) :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool

(.>=.) = Ge

(.<.) :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool

(.<.) = Lt

(.<=.) :: (Typeable a, Ord a) => Term k a -> Term k a -> Term k Bool
(.<=.) = Le

not' :: Term k Bool -> Term k Bool
not' = Not

(.&&.) :: Term k Bool -> Term k Bool -> Term k Bool
x .&&. (BoolLit True) = x
BoolLit True .&&. y  = y
x .&&. y = And x y

(.||.) :: Term k Bool -> Term k Bool -> Term k Bool
x .||. BoolLit False = x
BoolLit False .||. y  = y
x .||. y = Or x y

true :: Term k Bool
true = BoolLit True

false :: Term k Bool
false = BoolLit False

isIn :: (Typeable a, Eq a) => Term k a -> Term k [a] -> Term k Bool
isIn = IsIn

isNotIn :: (Typeable a, Eq a) => Term k a -> Term k [a] -> Term k Bool
isNotIn x xs = not' (x `isIn` xs)

length' :: Typeable a => Term k [a] -> Term k Integer
length' = Length

reverse' :: Typeable a => Term k [a] -> Term k [a]
reverse' = Reverse

sum' :: Term k [Integer] -> Term k Integer
sum' = Sum

product' :: Term k [Integer] -> Term k Integer
product' = Product

listLit :: (Show a, Typeable a) => [a] -> Term k [a]
listLit = ListLit

embeddedLit :: (Embeddable a, Typeable a, Show a) => a -> Term k (Embedded a)
embeddedLit = EmbeddedLit

-- do we still want to pre-define this?
filter' :: Term k1 (Integer -> Bool) -> Term k2 [Integer] -> Term 'PartiallyOpaque [Integer]
filter' = liftOpaque2 (filter, "filter")

liftOpaqueValue :: Typeable a => (a, String) -> Term 'PartiallyOpaque a
liftOpaqueValue (x,str) = Opaque (value str x) [] []

liftOpaque :: (Typeable a, Typeable b) => (a -> b, String) -> Term k a -> Term 'PartiallyOpaque b
liftOpaque (f,str) (Opaque x vs ts) = Opaque (value str f :$ x) vs ts
liftOpaque (f,str) x = Opaque (value str f :$ toExpr x) (termVarExps x) (transparentSubterms x)

liftOpaque2 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c, String) -> Term k1 a -> Term k2 b -> Term 'PartiallyOpaque c
liftOpaque2 (f,str) (Opaque x vx tx) (Opaque y vy ty) = Opaque (value str f :$ x :$ y) (vx ++ vy) (tx ++ ty)
liftOpaque2 (f,str) (Opaque x vx tx) y = Opaque (value str f :$ x :$ toExpr y) (vx ++ termVarExps y) (tx ++ transparentSubterms y)
liftOpaque2 (f,str) x (Opaque y vy ty) = Opaque (value str f :$ toExpr x :$ y) (termVarExps x ++ vy) (transparentSubterms x ++ ty)
liftOpaque2 (f,str) x y = Opaque (value str f :$ toExpr x :$ toExpr y) (termVarExps x ++ termVarExps y) (transparentSubterms x ++ transparentSubterms y)
