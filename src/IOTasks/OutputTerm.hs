{-# LANGUAGE TypeApplications #-}
module IOTasks.OutputTerm
  ( OutputTerm
  , eval
  ) where

import Prelude hiding (all)

import IOTasks.Terms
import IOTasks.Overflow

import Data.Express (Expr((:$)), var, val, value, (//-), evl)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Dynamic (Typeable)
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.Maybe (mapMaybe)

data OutputTerm a = OutputTerm Expr [ [Varname]]
  deriving (Eq,Ord,Show)

instance Accessor OutputTerm where
  currentValue' x 0 = OutputTerm (currentE x) [toVarList x]
  allValues' x 0 = OutputTerm (allE x) [toVarList x]

currentE :: VarExp a => a -> Expr
currentE x = var (show (toVarList x) ++ "_C") (undefined :: [I])

allE :: VarExp a => a -> Expr
allE x = var (show (toVarList x) ++ "_A") (undefined :: [I])

eval :: Typeable a => OutputTerm a -> Map Varname [(Integer,Int)] -> a
eval (OutputTerm t xss) e = evl (t //- concat [ [(currentE xs,val $ fromInteger @I $ head xs'),(allE xs,val $ map (fromInteger @I) xs')] | xs <- nub xss, let xs' = combinedVars xs ])
  where
    combinedVars :: [Varname] -> [Integer]
    combinedVars xs = (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) xs

--
instance Arithmetic OutputTerm where
  (.+.) = liftExpr2 $ value "(+)" ((+) :: I -> I -> I)
  (.-.) = liftExpr2 $ value "(-)" ((-) :: I -> I -> I)
  (.*.) = liftExpr2 $ value "(*)" ((*) :: I -> I -> I)
  intLit = (`OutputTerm` []) . val

instance BasicLists OutputTerm where
  length' = liftExpr1 $ value "length" (fromIntegral . length :: [I] -> I)
  sum' = liftExpr1 $ value "sum" (sum :: [I] -> I)
  product' = liftExpr1 $ value "product" (product :: [I] -> I)
  listLit = (`OutputTerm` []) . val

instance ComplexLists OutputTerm where
  filter' p = liftExpr1 $ value "filter p?" (filter p)

-- internal helpers
-- signatures are not ideal/wrong (DO NOT export these!)
liftExpr1 :: Expr -> OutputTerm a -> OutputTerm b
liftExpr1 f (OutputTerm x v) = OutputTerm (f :$ x) v

liftExpr2 :: Expr -> OutputTerm a -> OutputTerm b -> OutputTerm c
liftExpr2 f (OutputTerm x vx) (OutputTerm y vy) = OutputTerm (f :$ x :$ y) (vx++vy)
