module IOTasks.OutputTerm
  ( OutputTerm
  , current, all
  , eval
  --
  , (+#), (-#), (*#)
  , length', sum', product', filter'
  , intLit
  ) where

import Prelude hiding (all)

import IOTasks.Term (Varname, VarExp (toVarList))

import Data.Express (Expr((:$)), var, val, value, (//-), evl)
import Data.Map (Map)
import qualified Data.Map as Map (lookup)
import Data.Dynamic (Typeable)
import Data.List (sortBy, nub)
import Data.Function (on)
import Data.Maybe (mapMaybe)

data OutputTerm = OutputTerm Expr [[Varname]]
  deriving (Eq,Ord,Show)

current :: VarExp a => a -> OutputTerm
current x = OutputTerm (currentE x) [toVarList x]

all :: VarExp a => a -> OutputTerm
all x = OutputTerm (allE x) [toVarList x]

currentE :: VarExp a => a -> Expr
currentE x = var (show (toVarList x) ++ "_C") (undefined :: [Integer])

allE :: VarExp a => a -> Expr
allE x = var (show (toVarList x) ++ "_A") (undefined :: [Integer])

eval :: Typeable a => OutputTerm -> Map Varname [(Integer,Int)] -> a
eval (OutputTerm t xss) e = evl (t //- concat [ [(currentE xs,val $ head xs'),(allE xs,val xs')] | xs <- nub xss, let xs' = combinedVars xs ])
  where
    combinedVars :: [Varname] -> [Integer]
    combinedVars xs = (map fst . sortBy (flip compare `on` snd) . concat) $ mapMaybe (`Map.lookup` e) xs

--
(+#) :: OutputTerm -> OutputTerm -> OutputTerm
(+#) = liftExpr2 $ value "(+)" ((+) :: Integer -> Integer -> Integer)
(-#) :: OutputTerm -> OutputTerm -> OutputTerm
(-#) = liftExpr2 $ value "(-)" ((-) :: Integer -> Integer -> Integer)
(*#) :: OutputTerm -> OutputTerm -> OutputTerm
(*#) = liftExpr2 $ value "(*)" ((*) :: Integer -> Integer -> Integer)

length' :: OutputTerm -> OutputTerm
length' = liftExpr1 $ value "length" (fromIntegral . length :: [Integer] -> Integer)

sum' :: OutputTerm -> OutputTerm
sum' = liftExpr1 $ value "sum" (sum :: [Integer] -> Integer)

product' :: OutputTerm -> OutputTerm
product' = liftExpr1 $ value "product" (product :: [Integer] -> Integer)

intLit :: Integer -> OutputTerm
intLit = (`OutputTerm` []) . val

-- TODO: improve?
filter' :: (Integer -> Bool) -> OutputTerm -> OutputTerm
filter' p = liftExpr1 $ value "filter p?" (filter p)

liftExpr1 :: Expr -> OutputTerm -> OutputTerm
liftExpr1 f (OutputTerm x v) = OutputTerm (f :$ x) v

liftExpr2 :: Expr -> OutputTerm -> OutputTerm -> OutputTerm
liftExpr2 f (OutputTerm x vx) (OutputTerm y vy) = OutputTerm (f :$ x :$ y) (vx++vy)
