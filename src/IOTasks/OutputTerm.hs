module IOTasks.OutputTerm
  ( OutputTerm
  , current, all
  , eval
  --
  , (+#), (-#), (*#)
  , length', sum'
  ) where

import Prelude hiding (all)

import IOTasks.Term (Varname)

import Data.Express (Expr((:$)), var, val, value, (//-), evl)
import Data.Map (Map,toList)
import Data.Dynamic (Typeable)

newtype OutputTerm = OutputTerm Expr
  deriving (Eq,Ord,Show)

current :: String -> OutputTerm
current = OutputTerm . currentE

all :: String -> OutputTerm
all = OutputTerm . allE

currentE :: String -> Expr
currentE x = var (x ++ "_C") (undefined :: Integer)

allE :: String -> Expr
allE x = var (x ++ "_A") (undefined :: [Integer])

eval :: Typeable a => OutputTerm -> Map Varname [Integer] -> a
eval (OutputTerm t) e = evl (t //- concat [ [(currentE x,val $ head xs),(allE x,val xs)] | (x,xs) <- toList e])

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

liftExpr1 :: Expr -> OutputTerm -> OutputTerm
liftExpr1 f (OutputTerm x) = OutputTerm (f :$ x)

liftExpr2 :: Expr -> OutputTerm -> OutputTerm -> OutputTerm
liftExpr2 f (OutputTerm x) (OutputTerm y) = OutputTerm (f :$ x :$ y)
