{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module IOTasks.OutputTerm
  ( OutputTerm
  , eval
  ) where

import Prelude hiding (all)

import IOTasks.Terms
import IOTasks.Term hiding (eval)
import qualified IOTasks.Term as Term
import IOTasks.Overflow (OverflowWarning, checkOverflow, I)

import Data.Express (Expr((:$)), var, val, value, (//-), evl)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Typeable (eqT, (:~:)(..))
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.Maybe (mapMaybe)

data OutputTerm a
  = Transparent (Term a)
  | Opaque Expr [[Varname]] [SomeTerm]

toExpr :: OutputTerm a -> Expr
toExpr (Transparent t) = termExpr t
toExpr (Opaque expr _ _) = expr

-- simple instance liftig based on Expr's instances
instance Show (OutputTerm a) where
  show = show . toExpr

instance Eq (OutputTerm a) where
  (==) = (==) `on` toExpr

instance Ord (OutputTerm a) where
  compare = compare `on` toExpr

instance Accessor OutputTerm where
  currentValue' x 0 = Transparent $ Current x 0
  allValues' x 0 = Transparent $ All x 0

currentE :: VarExp a => a -> Expr
currentE x = var (show (toVarList x) ++ "_C") (undefined :: [Integer])

allE :: VarExp a => a -> Expr
allE x = var (show (toVarList x) ++ "_A") (undefined :: [Integer])

eval :: forall a. Overflow a => OutputTerm a -> Map Varname [(Integer,Int)] -> (OverflowWarning, a)
eval (Transparent t) e = Term.eval t e
eval (Opaque expr vss ts) e = let r = eval' expr vss e in case eqT @a @I of
  Just Refl -> (checkOverflow r,r)
  Nothing -> (foldMap (\(SomeTerm t) -> fst $ Term.eval t e) ts,r)
  where
  eval' :: Overflow a => Expr -> [[Varname]] -> Map Varname [(Integer,Int)] -> a
  eval' t xss e = evl (t //- concat [ [(currentE xs,val $ head xs'),(allE xs,val xs')] | xs <- nub xss, let xs' = combinedVars xs ])
    where
      combinedVars :: [Varname] -> [Integer]
      combinedVars xs = (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) xs

--
instance Arithmetic OutputTerm where
  (.+.) = h2 (.+.) $ value "(+)" ((+) :: Integer -> Integer -> Integer)
  (.-.) = h2 (.-.) $ value "(-)" ((-) :: Integer -> Integer -> Integer)
  (.*.) = h2 (.*.) $ value "(*)" ((*) :: Integer -> Integer -> Integer)
  intLit = Transparent . IntLitT . fromInteger

instance BasicLists OutputTerm where
  length' = h1 length' $ value "length" (fromIntegral . length :: [Integer] -> Integer)
  sum' = h1 sum' $ value "sum" (sum :: [Integer] -> Integer)
  product' = h1 product' $ value "product" (product :: [Integer] -> Integer)
  listLit = Transparent . ListLitT . map fromInteger

h1 :: (Term a -> Term b) -> Expr -> OutputTerm a -> OutputTerm b
h1 f _ (Transparent t) = Transparent $ f t
h1 _ g (Opaque x vs xs) = Opaque (g :$ x) vs xs

h2 :: (Term a -> Term b -> Term c) -> Expr -> OutputTerm a -> OutputTerm b -> OutputTerm c
h2 f _ (Transparent x) (Transparent y) = Transparent $ f x y
h2 _ g (Opaque x vx tx) (Transparent y) = Opaque (g :$ x :$ termExpr y) (vx ++ varExps y) (tx ++ subTerms y)
h2 _ g (Transparent x) (Opaque y vy ty) = Opaque (g :$ termExpr x :$ y) (varExps x ++ vy) (subTerms x ++ ty)
h2 _ g (Opaque x vx tx) (Opaque y vy ty) = Opaque (g :$ x :$ y) (vx ++ vy) (tx ++ ty)

termExpr :: Term a -> Expr
termExpr (termStruct -> Binary f x y) = binF f :$ termExpr x :$ termExpr y
termExpr (termStruct -> Unary f xs) = unaryF f :$ termExpr xs
termExpr (termStruct -> Var C x n) = currentE x
termExpr (termStruct -> Var A x n) = allE x
termExpr (termStruct -> Literal (BoolLit b)) = val b
termExpr (termStruct -> Literal (IntLit x)) = val x
termExpr (termStruct -> Literal (ListLit xs)) = val xs

unaryF :: UnaryF a b -> Expr
unaryF Not = value "not" (not :: Bool -> Bool)
unaryF Length = value "length" (fromIntegral . length :: [Integer] -> Integer)
unaryF Sum = value "sum" (sum :: [Integer] -> Integer)
unaryF Product = value "product" (product :: [Integer] -> Integer)

binF :: BinaryF a b c -> Expr
binF (:+:) = value "(+)" ((+) :: Integer -> Integer -> Integer)
binF (:-:) = value "(-)" ((-) :: Integer -> Integer -> Integer)
binF (:*:) = value "(*)" ((*) :: Integer -> Integer -> Integer)
binF (:==:) = value "(==)" ((==) :: Integer -> Integer -> Bool)
binF (:>:) = value "(>)" ((>) :: Integer -> Integer -> Bool)
binF (:>=:) = value "(>=)" ((>=) :: Integer -> Integer -> Bool)
binF (:<:) = value "(<)" ((<) :: Integer -> Integer -> Bool)
binF (:<=:) = value "(<=)" ((<=) :: Bool -> Bool -> Bool)
binF (:&&:) = value "(&&)" ((&&) :: Bool -> Bool -> Bool)
binF (:||:) = value "(||)" ((||) :: Bool -> Bool -> Bool)
binF IsIn = value "elem" (elem :: Integer -> [Integer] -> Bool)

instance ComplexLists OutputTerm where
  filter' p (Transparent x) = Opaque (value "filter p?" (filter p) :$ termExpr x) (varExps x) (subTerms x)
  filter' p (Opaque x vs ts) = Opaque (value "filter p?" (filter p) :$ x) vs ts
