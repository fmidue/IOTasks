{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module IOTasks.Z3 where

import IOTasks.Constraints
import IOTasks.ValueSet
import IOTasks.Term
import IOTasks.Terms (Var, VarExp(..), varname)
import IOTasks.ValueMap

import Z3.Monad

import Test.QuickCheck (generate)

import Control.Concurrent.STM

import Control.Monad.State

import Data.Maybe (catMaybes, fromMaybe, mapMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, sortOn)
import Data.Tuple.Extra (thd3)
import Data.Typeable

import Test.QuickCheck.Gen (Gen)
import Debug.Trace

type Timeout = Int

findPathInput :: Timeout -> Path -> Integer -> Bool -> IO (Maybe [String])
findPathInput t p bound checkOverflows = fst <$> findPathInputDebug t p bound checkOverflows

findPathInputDebug :: Timeout -> Path -> Integer -> Bool -> IO (Maybe [String],String)
findPathInputDebug t p bound checkOverflows = do
  evalZ3With Nothing (stdOpts +? opt "timeout" (show t)) $ pathScript p (WithSoft bound) checkOverflows

data SatResult = SAT | NotSAT deriving (Eq, Show)

isSatPath :: Timeout -> Path -> Bool -> IO SatResult
isSatPath t p checkOverflows = do
  (mRes,_) <- evalZ3With Nothing (stdOpts +? opt "timeout" (show t)) $ pathScript p WithoutSoft checkOverflows
  pure $ maybe NotSAT (const SAT) mRes

data ScriptMode = WithSoft Integer | WithoutSoft

data SearchContext = NoContext | LastNotSAT Int | RequirePruningCheck

updateContext :: SatResult -> Int -> SearchContext -> SearchContext
updateContext SAT _ _ = NoContext
updateContext NotSAT d NoContext = LastNotSAT d
updateContext NotSAT d (LastNotSAT d')
  | True = RequirePruningCheck
  | otherwise = LastNotSAT d
updateContext _ _ RequirePruningCheck = error "updateContext: should not happen"

type PrefixPath = Path
satPathsDebug :: Int -> Int -> ConstraintTree -> Bool -> IO [Path]
satPathsDebug n to t checkOverflows = do
  q <- atomically newTQueue
  nVar <- newTVarIO n
  satPaths nVar to t checkOverflows q
  map fromJust . init <$> atomically (flushTQueue q)

satPaths :: TVar Int -> Int -> ConstraintTree -> Bool -> TQueue (Maybe Path) -> IO ()
satPaths nVar to t checkOverflows q = do
  evalStateT (satPaths' 0 to t ([],0) q) NoContext
  atomically $ writeTQueue q Nothing
  where
    satPaths' :: Int -> Int -> ConstraintTree -> (PrefixPath,Int) -> TQueue (Maybe Path) -> StateT SearchContext IO ()
    satPaths' _ to Empty (s,d) q = do
      let path = reverse s -- constraints are stored reversed
      res <- lift $ isSatPath to path checkOverflows
      modify $ updateContext res d
      when (res == SAT) $ lift $ atomically $ writeTQueue q $ Just path
    satPaths' nReads to (Choice l r) (s,d) q = do
      satPaths' nReads to l (s,d+1) q
      ctx <- get
      case ctx of
        RequirePruningCheck -> do
          res <- lift $ isSatPath to s checkOverflows
          case res of
            NotSAT -> pure ()
            SAT -> do
              put NoContext
              satPaths' nReads to r (s,d+1) q
        _ -> do
          satPaths' nReads to r (s,d+1) q
    satPaths' nReads to (Assert c@InputConstraint{} t) (s,d) q = do
      n <- lift $ readTVarIO nVar
      if nReads > n
        then pure ()
        else satPaths' (nReads+1) to t (SomeConstraint c:s,d+1) q -- stores constraints in reversed order
    satPaths' nReads to (Assert c@ConditionConstraint{} t) (s,d) q = satPaths' nReads to t (SomeConstraint c:s,d+1) q -- stores constraints in reversed order
    satPaths' nReads to (Assert c@OverflowConstraints{} t) (s,d) q = satPaths' nReads to t (SomeConstraint c:s,d+1) q -- stores constraints in reversed order

-- path until next choice
lookAhead :: ConstraintTree -> Path
lookAhead Empty = []
lookAhead (Assert c t) = SomeConstraint c : lookAhead t
lookAhead Choice{} = []

data ValueGenerator where
  ValueGenerator :: Typeable v => (Integer -> Gen v) -> ValueGenerator

withGeneratedValue :: (forall v. Typeable v => v -> r) -> ValueGenerator -> Integer -> IO r
withGeneratedValue f (ValueGenerator g) bound = f <$> generate (g bound)

pathScript :: Path -> ScriptMode -> Bool -> Z3 (Maybe [String], String)
pathScript path mode checkOverflows = do
  let (tyConstr,predConstr,overflConstr) = partitionPath path
  vars <- forM tyConstr $
    \(InputConstraint ((x,ty),i) (vs :: ValueSet v)) -> do
      var <- mkFreshVar (x ++ show i) =<< mkSort @v
      constraint <- z3ValueSetConstraint vs var
      optimizeAssert constraint
      pure ((((x,ty),i),var),ValueGenerator $ valueOf vs)
  forM_ predConstr $
    \(ConditionConstraint t e) ->
      optimizeAssert =<< z3Predicate t e (map fst vars)

  when checkOverflows $
    forM_ overflConstr $
      \(OverflowConstraints ts e) ->
        forM_ ts (\t -> assertOverflowChecks t e (map fst vars))

  case mode of
    WithSoft bound -> do
      vs <- liftIO $ forM vars $ \((_,ast),gen) -> withGeneratedValue (\v -> (ast,wrapValue v)) gen bound
      def <- mkStringSymbol "default"
      forM_ vs $ \(ast,v) -> do
        eq <- mkEq ast =<< mkValueRep v
        optimizeAssertSoft eq "1" def -- soft assert with weight 1 and id "default"
    WithoutSoft -> pure ()
  str <- optimizeToString
  result <- optimizeCheck []
  mRes <- case result of
    Sat -> do
      model <- optimizeGetModel
      Just . catMaybes <$> mapM ((evalAST model . snd) . fst) vars
    _ -> do
      pure Nothing
  pure (mRes,str)

evalAST :: Model -> AST -> Z3 (Maybe String)
evalAST m x = do
  isS <- isStringSort =<< getSort x
  if isS
    then do
      mR <- modelEval m x True
      case mR of
        Just r -> Just <$> getString r
        Nothing -> pure Nothing
    else fmap show <$> evalInt m x

sortTest :: IO Result
sortTest = evalZ3 $ do
  x <- mkFreshVar "x" =<< mkStringSort
  hi <- mkString "Hi"
  eq <- mkEq x hi
  def <- mkStringSymbol "default"
  _ <- optimizeAssertSoft eq "1" def
  optimizeCheck []

mkSort :: forall a z3. (Typeable a, MonadZ3 z3) => z3 Sort
mkSort =
  case eqT @a @Integer of
    Just Refl -> mkIntSort
    Nothing -> case eqT @a @String of
      Just Refl -> mkStringSort
      Nothing -> error "mkSort: unsupported type"

mkValueRep :: Value -> Z3 AST
mkValueRep (IntegerValue n) = mkInteger n
mkValueRep (StringValue s) = mkString s

z3Predicate :: Term a -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
z3Predicate (termStruct -> Binary IsIn x (All y n)) e vars = do
  xP <- z3Predicate x e vars
  mkOr =<< mapM (mkEq xP . fromMaybe (unknownVariablesError y) . (`lookup` vars)) (weaveVariables y n e)
z3Predicate (termStruct -> Binary IsIn x (ListLitT xs)) e vars = do
  xP <- z3Predicate x e vars
  mkOr =<< mapM (mkIntNum >=> mkEq xP) xs
z3Predicate (termStruct -> Binary f x y) e vars = binRec e vars (binF f) x y where
  binF :: BinaryF a b c -> AST -> AST -> Z3 AST
  binF (:+:) = \a b -> mkAdd [a,b]
  binF (:-:) = \a b -> mkSub [a,b]
  binF (:*:) = \a b -> mkMul [a,b]
  binF (:==:) = mkEq
  binF (:>:) = mkGt
  binF (:>=:) = mkGe
  binF (:<:) = mkLt
  binF (:<=:) = mkLe
  binF (:&&:) = \a b -> mkAnd [a,b]
  binF (:||:) = \a b -> mkOr [a,b]
  binF _ = error ""
z3Predicate (termStruct -> Unary Not x) e vars = mkNot =<< z3Predicate x e vars
z3Predicate (termStruct -> Unary Length (All x n)) e _ = mkIntNum $ length $ weaveVariables x n e
z3Predicate (termStruct -> Unary Sum (All x n)) e vars = mkAdd $ lookupList (weaveVariables x n e) vars
z3Predicate (termStruct -> Unary Product (All x n)) e vars = mkMul $ lookupList (weaveVariables x n e) vars
z3Predicate (termStruct -> Unary Length (ListLitT xs)) _ _ = mkIntNum $ length xs
z3Predicate (termStruct -> Unary Sum (ListLitT xs)) _ _ = mkIntNum $ sum xs
z3Predicate (termStruct -> Unary Product (ListLitT xs)) _ _ = mkIntNum $ product xs
z3Predicate (termStruct -> Variable C x n) e vars = pure $ fromMaybe (unknownVariablesError x) $ (`lookup` vars) . last $ weaveVariables x n e
z3Predicate (termStruct -> Variable A _x _) _e _vars = error "generic list"
z3Predicate (termStruct -> Literal (IntLit n)) _ _ = mkIntNum n
z3Predicate (termStruct -> Literal (ListLit _)) _ _ = error "generic list literal"
z3Predicate (termStruct -> Literal (BoolLit b)) _ _ = mkBool b

unknownVariablesError :: VarExp a => a -> b
unknownVariablesError x = error $ "unknown variable(s) {" ++ intercalate "," (map varname $ toVarList x) ++ "}"

-- helper for binary recursive case
binRec :: Map Var (Int,[Int]) -> [((Var,Int),AST)] -> (AST -> AST -> Z3 AST) -> Term a -> Term b -> Z3 AST
binRec e vs f x y = do
  xP <- z3Predicate x e vs
  yP <- z3Predicate y e vs
  f xP yP

weaveVariables :: VarExp a => a -> Int -> Map Var (Int,[Int]) -> [(Var,Int)]
weaveVariables vs n e =
    reverse . drop n . reverse -- drop last n variables
  . map (\(x,y,_) -> (x,y))
  . sortOn thd3
  . concatMap (\(x,(i,ks)) -> [(x,j,k) | (j,k) <- zip [i,i-1..1] ks ])
  . mapMaybe (\v -> (v,) <$> Map.lookup v e)
  $ toVarList vs

lookupList :: Eq a => [a] -> [(a, b)] -> [b]
lookupList vs vars = mapMaybe (`lookup` vars) vs

z3ValueSetConstraint :: ValueSet a -> AST -> Z3 AST
z3ValueSetConstraint (Union x y) xVar = do
  cx <- z3ValueSetConstraint x xVar
  cy <- z3ValueSetConstraint y xVar
  mkOr [cx,cy]
z3ValueSetConstraint (Intersection x y) xVar = do
  cx <- z3ValueSetConstraint x xVar
  cy <- z3ValueSetConstraint y xVar
  mkAnd [cx,cy]
z3ValueSetConstraint (GreaterThan n) xVar = mkIntNum n >>= mkGt xVar
z3ValueSetConstraint (LessThen n) xVar = mkIntNum n >>= mkLt xVar
z3ValueSetConstraint (Eq n) xVar = mkIntNum n >>= mkEq xVar
z3ValueSetConstraint Every _ = mkTrue
z3ValueSetConstraint None _ = mkFalse

assertOverflowChecks :: Term Integer ->  Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 ()
assertOverflowChecks t e vars = do
  ast <- z3Predicate t e vars
  overflowConstraints ast

overflowConstraints :: AST -> Z3 ()
overflowConstraints x = do
  minInt <- mkInteger (toInteger $ minBound @Int)
  maxInt <- mkInteger (toInteger $ maxBound @Int)
  optimizeAssert =<< mkGe x minInt
  optimizeAssert =<< mkLe x maxInt
