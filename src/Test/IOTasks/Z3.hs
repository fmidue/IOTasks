{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.Z3 (findPathInput, printPathScript, evalPathScript, satPaths, satPathsQ, isSatPath, SatResult(..), Timeout) where

import Test.IOTasks.Constraints
import Test.IOTasks.ValueSet
import Test.IOTasks.Internal.ConditionTerm
import Test.IOTasks.Terms (Var(..), SomeVar, pattern SomeVar, VarExp(..), someVarname)
import Test.IOTasks.ValueMap

import Z3.Monad
import Z3.Base (Goal)

import Test.QuickCheck (generate)

import Control.Concurrent.STM

import Control.Monad.State

import Data.Maybe (catMaybes, fromMaybe, mapMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple.Extra (thd3)

import Data.List as List
import Test.QuickCheck.Gen (Gen)
import Type.Reflection
import Data.Either (fromRight)
import Type.Match
import Data.List.Extra
import Control.Monad.Reader

type Z3R = ReaderT ImplicitParameters Z3

data ImplicitParameters = ImplicitParameter { valueSizeParameter :: Integer, maxSeqLengthParameter :: Int }

type Timeout = Int

findPathInput :: Timeout -> Path -> Integer -> Int -> Bool -> IO (SatResult [String])
findPathInput t p bound maxSeqLength checkOverflows = fst <$> evalPathScript t p bound maxSeqLength checkOverflows

printPathScript :: Timeout -> Path -> Integer -> Int -> Bool -> IO String
printPathScript t p bound maxSeqLength checkOverflows = snd <$> evalPathScript t p bound maxSeqLength checkOverflows

evalPathScript :: Timeout -> Path -> Integer -> Int -> Bool -> IO (SatResult [String],String)
evalPathScript t p bound maxSeqLength checkOverflows = do
  evalZ3With Nothing (stdOpts +? opt "timeout" (show t)) $ runReaderT (pathScript p WithSoft checkOverflows) implicits
  where implicits = ImplicitParameter {valueSizeParameter=bound, maxSeqLengthParameter=maxSeqLength}

data SatResult a = SAT a | NotSAT | Timeout deriving (Eq, Show, Functor)

isSatPath :: Timeout -> Path-> Int -> Bool -> IO (SatResult ())
isSatPath t p maxSeqLength checkOverflows = do
  (mRes,_) <- evalZ3With Nothing (stdOpts +? opt "timeout" (show t)) $ runReaderT (pathScript p WithoutSoft checkOverflows) implicits
  pure $ void mRes
  where implicits = ImplicitParameter {valueSizeParameter= error "isSatPath: valueSize not available in isSatPath", maxSeqLengthParameter=maxSeqLength}

data ScriptMode = WithSoft | WithoutSoft

data SearchContext = NoContext | LastNotSAT Int | RequirePruningCheck

-- depth is currently not used to trigger pruning
updateContext :: SatResult a -> Int -> SearchContext -> SearchContext
updateContext (SAT _) _ _ = NoContext
updateContext Timeout _ _ = NoContext
updateContext NotSAT d NoContext = LastNotSAT d
updateContext NotSAT _ (LastNotSAT _) = RequirePruningCheck
updateContext _ _ RequirePruningCheck = error "updateContext: should not happen"

type PrefixPath = Path
satPaths :: Int -> Int -> ConstraintTree -> Int -> Bool -> IO [Path]
satPaths maxUnfolds to t maxSeqLength checkOverflows = do
  q <- atomically newTQueue
  nVar <- newTVarIO Nothing
  satPathsQ nVar to t maxUnfolds maxSeqLength checkOverflows q
  map fromJust . init <$> atomically (flushTQueue q)

satPathsQ :: TVar (Maybe Int) -> Int -> ConstraintTree -> Int -> Int -> Bool -> TQueue (Maybe Path) -> IO ()
satPathsQ nVar to t maxUnfolds maxSeqLength checkOverflows q = do
  evalStateT (satPaths' 0 0 to t ([],0) q) NoContext
  atomically $ writeTQueue q Nothing
  where
    -- nUnfolds: number of Unfolds on current path
    -- nInputs: number of inputs on current path (path length)
    -- to: solver timeout
    -- (s,d): current prefix path + depth of current position in the tree (d =/= path length)
    satPaths' :: Int -> Int -> Int -> ConstraintTree -> (PrefixPath,Int) -> TQueue (Maybe Path) -> StateT SearchContext IO ()
    satPaths' _ _ to Empty (s,d) q = do
      let path = reverse s -- constraints are stored reversed
      res <- lift $ isSatPath to path maxSeqLength checkOverflows
      modify $ updateContext res d
      when (res == SAT ()) $ lift $ atomically $ writeTQueue q $ Just path
    satPaths' nUnfolds nInputs to (Choice l r) (s,d) q = do
      satPaths' nUnfolds nInputs to l (s,d+1) q
      ctx <- get
      case ctx of
        RequirePruningCheck -> do
          res <- lift $ isSatPath to s maxSeqLength checkOverflows
          case res of
            NotSAT -> pure ()
            Timeout -> do
              put NoContext
              satPaths' nUnfolds nInputs to r (s,d+1) q
            SAT () -> do
              put NoContext
              satPaths' nUnfolds nInputs to r (s,d+1) q
        _ -> do
          satPaths' nUnfolds nInputs to r (s,d+1) q
    satPaths' nUnfolds nInputs to (Assert c@InputConstraint{} t) (s,d) q = do
      currentMaxPathLength <- lift $ readTVarIO nVar
      case currentMaxPathLength of
        Just len | nInputs >= len -> pure ()
        _ -> satPaths' nUnfolds (nInputs+1) to t (SomeConstraint c:s,d+1) q -- stores constraints in reversed order
    satPaths' nUnfolds nInputs to (Assert c t) (s,d) q = satPaths' nUnfolds nInputs to t (SomeConstraint c:s,d+1) q -- stores constraints in reversed order
    satPaths' nUnfolds nInputs to (Unfold t) (s,d) q
      | nUnfolds < maxUnfolds = satPaths' (nUnfolds+1) nInputs to t (s,d) q
      | otherwise = pure ()

data ValueGenerator where
  ValueGenerator :: Typeable v => (Size -> Gen v) -> ValueGenerator

withGeneratedValue :: (forall v. Typeable v => v -> r) -> ValueGenerator -> Size -> IO r
withGeneratedValue f (ValueGenerator g) sz = f <$> generate (g sz)

pathScript :: Path -> ScriptMode -> Bool -> Z3R (SatResult [String], String)
pathScript path mode checkOverflows = do
  let (tyConstr,predConstr,overflConstr) = partitionPath path

  goal <- mkGoal True False False
  vars <- forM tyConstr $
    \(InputConstraint (Var (x,ty),i) (vs :: ValueSet v)) -> do
      var <- mkFreshVar (x ++ show i) =<< mkSort @v
      constraint <- z3ValueSetConstraint vs var
      goalAssert goal constraint
      pure (((SomeVar (x,SomeTypeRep ty),i),var),ValueGenerator $ valueOf vs)
  forM_ predConstr $
    \(ConditionConstraint t e) ->
        goalAssert goal =<< z3Predicate goal t e (map fst vars)

  when checkOverflows $
    forM_ overflConstr $
      \(OverflowConstraints ts e) ->
        forM_ ts (\t -> assertOverflowChecks goal t e (map fst vars))

  -- simplify and assert goal
  simplifyTactic <- mkTactic "simplify"
  tacticResult <- applyTactic simplifyTactic goal
  [newGoal] <- getApplyResultSubgoals tacticResult
  fs <- getGoalFormulas newGoal
  forM_ fs $ \f -> lift $ optimizeAssert f

  case mode of
    WithSoft -> do
      ImplicitParameter {..} <- ask
      vs <- liftIO $ forM vars $ \((_,ast),gen) -> withGeneratedValue (\v -> (ast,wrapValue v)) gen (Size valueSizeParameter (maxSeqLengthParameter `div` 2))
      forM_ vs $ \(ast,v) -> do
        cs <- mkValueRep ast v
        forM cs $ \(c,l) -> lift $ optimizeAssertSoft c "1" l -- soft assert with weight 1 and id "default"
    WithoutSoft -> pure ()
  str <- lift optimizeToString
  result <- lift $ optimizeCheck []
  mRes <- case result of
    Sat -> do
      model <- lift optimizeGetModel
      SAT . catMaybes <$> mapM ((evalAST model . snd) . fst) vars
    Unsat -> do
      pure NotSAT
    Undef -> pure Timeout
  pure (mRes,str)

evalAST :: MonadZ3 z3 => Model -> AST -> z3 (Maybe String)
evalAST m x = do
  isS <- isStringSort =<< getSort x
  if isS
    then do
      mR <- modelEval m x True
      case mR of
        Just r -> Just <$> getString r
        Nothing -> pure Nothing
    else fmap show <$> evalInt m x

mkSort :: forall a z3. (Typeable a, MonadZ3 z3) => z3 Sort
mkSort =
  case eqTypeRep (typeRep @a) (typeRep @Integer) of
    Just HRefl -> mkIntSort
    Nothing -> case eqTypeRep (typeRep @a) (typeRep @String) of
      Just HRefl -> mkStringSort
      Nothing -> error "mkSort: unsupported type"

mkValueRep :: MonadZ3 z3 => AST -> Value -> z3 [(AST,Symbol)]
mkValueRep x (IntegerValue n) = do
  def <- mkStringSymbol "default"
  pure . (,def) <$> (mkEq x =<< mkInteger n)
mkValueRep x (StringValue s) = do
  xStr <- astToString x
  -- length constraint
  lenSym <- mkStringSymbol $ xStr ++ "_len"
  len <- mkIntNum $ length s
  lenEq <- mkEq len =<< mkSeqLength x
  -- character range constraint
  rangeSym <- mkStringSymbol $ xStr ++ "_range"
  r1 <- reRange 'a' 'z'
  r2 <- reRange 'A' 'Z'
  rangeEq <- mkSeqInRe x =<< mkReStar =<< mkReUnion (2 :: Integer) [r1,r2]
  -- value constraints
  vals <- mapM (positionConstraint xStr) (s `zip` [1..])
  pure $ (lenEq,lenSym) : (rangeEq,rangeSym) : vals
  where
    reRange a b = do
      aStr <- mkString [a]
      bStr <- mkString [b]
      mkReRange aStr bStr
    positionConstraint xStr (c,i) = do
      xi <- mkSeqAt x =<< mkInteger i
      eq <- mkEq xi =<< mkString [c]
      sym <- mkStringSymbol $ xStr ++ "_val"
      pure (eq,sym)

z3Predicate :: Goal -> ConditionTerm x -> Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
z3Predicate goal (termStruct -> Binary f x y) e vars = z3PredicateBinary goal f x y e vars
z3Predicate goal (termStruct -> Unary f x) e vars = z3PredicateUnary goal f x e vars
z3Predicate _ (termStruct -> VariableC x n) e vars = pure $ fromMaybe (unknownVariablesError x) $ (`List.lookup` vars) . last $ weaveVariables x n e
z3Predicate _ (termStruct -> Literal (IntLit n)) _ _ = mkIntNum n
z3Predicate _ (termStruct -> Literal (BoolLit b)) _ _ = mkBool b
--
z3Predicate _ (termStruct -> VariableA _x _) _e _vars = error "z3Predicate: top level list should not happen"
z3Predicate _ (termStruct -> Literal (ListLit _)) _ _ = error "z3Predicate: top level list literal should not happen"

z3PredicateUnary :: forall a b. Typeable a => Goal -> UnaryF a b -> ConditionTerm a ->  Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
z3PredicateUnary goal f x e vars = case typeRep @a of
  App c _ -> case eqTypeRep c (typeRep @[]) of
    Just HRefl -> unaryListA goal f x e vars  -- a ~ [a1]
    Nothing -> unaryNoList goal f x e vars-- a ~ f a1
  _ -> unaryNoList goal f x e vars --

unaryListA :: Goal -> UnaryF [a] b -> ConditionTerm [a] -> Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
unaryListA _ Length (Current x n) e vars = mkSeqLength . fromJust . (`List.lookup` vars) . last $ weaveVariables x n e --special case for string variables
unaryListA goal Length xs e vars = unaryListRec goal (mkIntNum . length @[]) xs e vars
unaryListA _ Reverse _ _ _ = error "z3Predicate: top level reverse should not happen"
unaryListA goal Sum xs e vars = unaryListRec goal mkAdd xs e vars
unaryListA goal Product xs e vars = unaryListRec goal mkMul xs e vars

unaryNoList :: Goal -> UnaryF a b -> ConditionTerm a -> Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
unaryNoList goal Not x e vars = mkNot =<< z3Predicate goal x e vars
unaryNoList _ _ _ _ _ = error "handled by unaryListA"

unaryListRec :: Typeable a => Goal -> ([AST] -> Z3R AST) -> ConditionTerm [a] -> Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
unaryListRec goal f xs e vars = f . fromRight (error "unexpected result") =<< listASTs goal xs e vars

z3PredicateBinary :: forall a b c. (Typeable a, Typeable b) => Goal -> BinaryF a b c -> ConditionTerm a -> ConditionTerm b ->  Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R AST
z3PredicateBinary goal f x y e vars = case typeRep @a of
  App ca _ -> case eqTypeRep ca (typeRep @[]) of
    Just HRefl -> case typeRep @b of
      App cb _ -> case eqTypeRep cb (typeRep @[]) of
        Just HRefl ->  case1 f x y -- a ~ [a1], b ~ [b1]
        Nothing -> case2 f x y -- a ~ [a1], b ~ f b1
      _ -> case2 f x y -- a ~ [a1]
    Nothing -> case typeRep @b of
      App cb _ -> case eqTypeRep cb (typeRep @[]) of
        Just HRefl ->  case3 f x y -- a ~ f a1, b ~ [b1]
        Nothing -> case4 f x y -- a ~ f a1, b ~ f b1
      _ -> case4 f x y-- a ~ f a1
  _ -> case typeRep @b of
    App cb _ -> case eqTypeRep cb (typeRep @[]) of
      Just HRefl ->  case3 f x y -- b ~ [b1]
      Nothing -> case4 f x y -- b ~ f b1
    _ -> case4 f x y --
  where
  case1 :: forall a b c. (Typeable [a], Typeable [b]) => BinaryF [a] [b] c -> ConditionTerm [a] -> ConditionTerm [b] -> Z3R AST
  case1 f x y = do
    rx <- listASTs goal x e vars
    ry <- listASTs goal y e vars
    case (rx,ry) of
      (Right xs,Right ys) -> binListAB f xs ys
      (Right xs,Left y) -> binListA f xs y
      (Left x,Right ys) -> binListB f x ys
      (Left x,Left y) -> binNoList f x y
  case2 :: forall a b c. Typeable [a] => BinaryF [a] b c -> ConditionTerm [a] -> ConditionTerm b -> Z3R AST
  case2 f x y = do
    exs <- listASTs goal x e vars
    y' <- z3Predicate goal y e vars
    case exs of
      Right xs -> binListA f xs y'
      Left x -> binNoList f x y'
  case3 :: forall a b c. Typeable [b] => BinaryF a [b] c -> ConditionTerm a -> ConditionTerm [b] -> Z3R AST
  case3 f x y = do
    x' <- z3Predicate goal x e vars
    eys <- listASTs goal y e vars
    case eys of
      Right ys -> binListB f x' ys
      Left y -> binNoList f x' y
  case4 :: forall a b c. BinaryF a b c -> ConditionTerm a -> ConditionTerm b -> Z3R AST
  case4 f x y = do
    xP <- z3Predicate goal x e vars
    yP <- z3Predicate goal y e vars
    binNoList f xP yP

binNoList :: forall z3 a b c. MonadZ3 z3 => BinaryF a b c -> AST -> AST -> z3 AST
binNoList (:+:) = \a b -> mkAdd [a,b]
binNoList (:-:) = \a b -> mkSub [a,b]
binNoList (:*:) = \a b -> mkMul [a,b]
binNoList (:==:) = mkEq
binNoList (:>:) = matchType @a [inCaseOf' @String $ (\a b -> mkNot =<< mkStrLe a b), fallbackCase' mkGt]
binNoList (:>=:) = matchType @a [inCaseOf' @String $ (\a b -> mkNot =<< mkStrLt a b), fallbackCase' mkGe]
binNoList (:<:) = matchType @a [inCaseOf' @String $ mkStrLt, fallbackCase' mkLt]
binNoList (:<=:) = matchType @a [inCaseOf' @String $ mkStrLe, fallbackCase' mkLe]
binNoList (:&&:) = \a b -> mkAnd [a,b]
binNoList (:||:) = \a b -> mkOr [a,b]
binNoList IsIn = error "handled by binListB"

binListA :: MonadZ3 z3 => BinaryF [a] b c -> [AST] -> AST -> z3 AST
binListA = error "does not happen with currently supported functions"

binListB :: MonadZ3 z3 => BinaryF a [b] c -> AST -> [AST] -> z3 AST
binListB IsIn x ys = mkOr =<< mapM (mkEq x) ys
binListB _ _ _ = error "all other functions are handled by binListAB"

binListAB :: MonadZ3 z3 => BinaryF [a] [b] c -> [AST] -> [AST] -> z3 AST
binListAB (:==:) = compareSymbolic $ Strict EQ
binListAB (:>:)  = compareSymbolic $ Strict GT
binListAB (:>=:) = compareSymbolic $ ReflexiveClosure GT
binListAB (:<:) = compareSymbolic $ Strict LT
binListAB (:<=:) = compareSymbolic $ ReflexiveClosure LT

data CompareOp = Strict Ordering | ReflexiveClosure Ordering

compareSymbolic :: MonadZ3 z3 => CompareOp -> [AST] -> [AST] -> z3 AST
compareSymbolic cmpOp xs ys = do
  let (mkCmp,mkConstShort,mkConstLong) = compareParams cmpOp
  strictComps <- mapM mkAnd =<< clauses =<< compareASTLists mkCmp mkConstShort mkConstLong
  case cmpOp of
    Strict{} -> mkOr strictComps
    ReflexiveClosure{} -> do
      eqComp <- mkAnd =<< compareASTLists mkEq mkFalse mkFalse
      mkOr $ eqComp : strictComps
  where
    compareParams :: MonadZ3 z3 => CompareOp -> (AST -> AST -> z3 AST, z3 AST, z3 AST)
    compareParams (Strict EQ) = (mkEq,mkFalse,mkFalse)
    compareParams (Strict LT) = (mkLt,mkTrue,mkFalse)
    compareParams (Strict GT) = (mkGt,mkFalse,mkTrue)
    compareParams (ReflexiveClosure x) = compareParams $ Strict x
    compareASTLists :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> z3 AST -> z3 AST -> z3 [AST]
    compareASTLists mkCmp mkConstShort mkConstLong = sequence (zipWithLongest (comparePositions mkCmp mkConstShort mkConstLong) xs ys)
    comparePositions :: MonadZ3 z3 => (AST -> AST -> z3 AST) -> z3 AST -> z3 AST -> Maybe AST -> Maybe AST -> z3 AST
    comparePositions mkCmp _ _ (Just x) (Just y) = mkIntermediateBoolean =<< mkCmp x y
    comparePositions _ mkConstShort _ Nothing (Just _) = mkConstShort
    comparePositions _ _ mkConstLong (Just _) Nothing = mkConstLong
    comparePositions _ _ _ Nothing Nothing = error "impossible: invariant of zipWithLongest"
    clauses :: MonadZ3 z3 => [AST] -> z3 [[AST]]
    clauses [] = pure []
    clauses [b1] = pure [[b1]]
    clauses (b:bs) = do
      nb <- mkNot b
      r <- clauses bs
      pure $ [b] : ((nb :) <$> r)

mkIntermediateBoolean :: MonadZ3 z3 => AST -> z3 AST
mkIntermediateBoolean x = do
  b <- mkFreshBoolVar "b"
  mkEq b x

listASTs :: forall a. Typeable [a] => Goal -> ConditionTerm [a] -> Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R (Either AST [AST])
listASTs goal (ReverseT (ReverseT t)) e vars = listASTs goal t e vars
listASTs goal (ReverseT t) e vars = do
  r <- listASTs goal t e vars
  case r of
    Right as -> pure $ Right $ reverse as
    Left a -> Left <$> reverseSequence goal a
listASTs _ (ListLitT xs) _ _ =
  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Right <$> mapM (mkIntNum . toInteger) xs
    , inCaseOfE' @Char $ \HRefl -> Left <$> mkString xs
    , fallbackCase' $ error $ "list literal of unsupported type " ++ show (typeRep @[a])
    ]
listASTs _ (All x n) e vars = pure . Right $ lookupList (weaveVariables x n e) vars
listASTs _ (Current x n) e vars = pure . Left . head $ lookupList (weaveVariables x n e) vars

reverseSequence :: Goal -> AST -> Z3R AST
reverseSequence goal x = do
  y <- mkFreshVar "reversed" =<< getSort x
  lx <- mkSeqLength x
  ly <- mkSeqLength y
  lift (goalAssert goal =<< mkEq lx ly)
  m <- asks maxSeqLengthParameter
  lift (goalAssert goal =<< mkLe lx =<< mkIntNum m)
  forM_ [0 .. m] (lift . goalAssert goal <=< pos (x,lx) (y,ly))
  pure y
  where
    pos :: MonadZ3 z3 => (AST,AST) -> (AST,AST) -> Int -> z3 AST
    pos (x,lx) (y,ly) i = do
      index <- mkIntNum i
      pre <- mkGe lx index
      xi <- mkSeqAt x index
      index' <- mkIntNum (i+1)
      yj <- mkSeqAt y =<< mkSub [ly,index']
      con <- mkEq xi yj
      mkImplies pre con

unknownVariablesError :: VarExp a => a -> b
unknownVariablesError x = error $ "unknown variable(s) {" ++ intercalate "," (map someVarname $ toVarList x) ++ "}"

weaveVariables :: VarExp a => a -> Int -> Map SomeVar (Int,[Int]) -> [(SomeVar,Int)]
weaveVariables vs n e =
    reverse . drop n . reverse -- drop last n variables
  . map (\(x,y,_) -> (x,y))
  . sortOn thd3
  . concatMap (\(x,(i,ks)) -> [(x,j,k) | (j,k) <- zip [i,i-1..1] ks ])
  . mapMaybe (\v -> (v,) <$> Map.lookup v e)
  $ toVarList vs

lookupList :: Eq a => [a] -> [(a, b)] -> [b]
lookupList vs vars = mapMaybe (`List.lookup` vars) vs

z3ValueSetConstraint :: MonadZ3 z3 => ValueSet a -> AST -> z3 AST
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

assertOverflowChecks :: Goal -> ConditionTerm Integer ->  Map SomeVar (Int,[Int]) -> [((SomeVar, Int), AST)] -> Z3R ()
assertOverflowChecks goal t e vars = do
  ast <- z3Predicate goal t e vars
  overflowConstraints goal ast

overflowConstraints :: Goal -> AST -> Z3R ()
overflowConstraints goal x = do
  minInt <- mkInteger (toInteger $ minBound @Int)
  maxInt <- mkInteger (toInteger $ maxBound @Int)
  lift (goalAssert goal =<< mkGe x minInt)
  lift (goalAssert goal =<< mkLe x maxInt)
