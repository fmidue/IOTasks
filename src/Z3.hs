{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
module Z3 where

import Constraints
import ValueSet

import Z3.Monad

import Test.QuickCheck (chooseInteger, generate, vectorOf)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes, fromMaybe)
import Term
import Data.Map (Map)
import qualified Data.Map as Map

findPathInput :: Path -> Integer -> IO (Maybe [Integer])
findPathInput p bound = evalZ3 $ pathScript p bound

pathScript :: Path -> Integer -> Z3 (Maybe [Integer])
pathScript path bound = do
  let (tyConstr,predConstr) = partitionPath path
  vars <- forM tyConstr $
    \(InputConstraint (x,i) vs) -> do
      var <- mkFreshIntVar $ x ++ show i
      constraint <- z3ValueSetConstraint vs var
      optimizeAssert constraint
      pure ((x,i),var)
  forM_ predConstr $
    \(ConditionConstraint t e) ->
      optimizeAssert =<< z3Predicate t e vars
  vs <- liftIO $ generate $ vectorOf (length vars) $ chooseInteger (-bound,bound)
  def <- mkStringSymbol "default"
  forM_ (zip (zip vs vars) [1..]) $ \((v,(_,x)),_w) -> do
    eq <- mkEq x =<< mkInteger v
    optimizeAssertSoft eq "1" def -- soft assert with weight 1 and id "default"
  result <- optimizeCheck []
  case result of
    Sat -> do
      model <- optimizeGetModel
      Just . catMaybes <$> mapM (evalInt model . snd) vars
    _ -> do
      str <- optimizeToString
      -- liftIO $ print str
      pure Nothing

z3Predicate :: Term a -> Map Varname Int -> [((Varname, Int), AST)] -> Z3 AST
z3Predicate (x :+: y) e vars = binRec e vars (\a b -> mkAdd [a,b]) x y
z3Predicate (x :-: y) e vars = binRec e vars (\a b -> mkSub [a,b]) x y
z3Predicate (x :*: y) e vars = binRec e vars (\a b -> mkMul [a,b]) x y
z3Predicate (x :==: y) e vars = binRec e vars mkEq x y
z3Predicate (x :>: y) e vars = binRec e vars mkGt x y
z3Predicate (Not x) e vars = mkNot =<< z3Predicate x e vars
z3Predicate (Length (All x)) e _ = mkIntNum $ fromMaybe 0 $ Map.lookup x e
z3Predicate (Sum (All x)) e vars = let n = fromMaybe 0 $ Map.lookup x e in mkAdd [ xi | ((name,i),xi) <- vars, name == x, i <= n ]
z3Predicate (Current x) e vars = pure $ fromMaybe (error $ "unknown variable " ++ x++ show e ++ show vars) $ Map.lookup x e >>= (\i -> lookup (x,i) vars)
z3Predicate (All _x) _e _vars = error "generic list"

-- helper for binary recursive case
binRec :: Map Varname Int -> [((Varname,Int),AST)] -> (AST -> AST -> Z3 AST) -> Term a -> Term a -> Z3 AST
binRec e vs f x y = do
  xP <- z3Predicate x e vs
  yP <- z3Predicate y e vs
  f xP yP

z3ValueSetConstraint :: ValueSet -> AST -> Z3 AST
z3ValueSetConstraint (Union x y) xVar = do
  cx <- z3ValueSetConstraint x xVar
  cy <- z3ValueSetConstraint y xVar
  mkOr [cx,cy]
z3ValueSetConstraint (Intersection x y) xVar = do
  cx <- z3ValueSetConstraint x xVar
  cy <- z3ValueSetConstraint y xVar
  mkAnd [cx,cy]
z3ValueSetConstraint (Complement x) xVar = mkNot =<< z3ValueSetConstraint x xVar
z3ValueSetConstraint (GreaterThan n) xVar = mkIntNum n >>= mkGt xVar
z3ValueSetConstraint (LessThen n) xVar = mkIntNum n >>= mkLt xVar
z3ValueSetConstraint (Eq n) xVar = mkIntNum n >>= mkEq xVar
z3ValueSetConstraint Every _ = mkTrue
