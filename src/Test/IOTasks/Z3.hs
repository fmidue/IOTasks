{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE KindSignatures #-}
module Test.IOTasks.Z3 (
  findPathInput, printPathScript, evalPathScript,
  satPaths, satPathsQ, isSatPath, SatResult(..), Timeout,
  PathInjector(..), mkPathInjector,
  ) where

import Test.IOTasks.Constraints
import Test.IOTasks.Internal.ValueSet
import Test.IOTasks.Internal.Term
import Test.IOTasks.Var (Var(..), SomeVar, pattern SomeVar, pattern Var, VarExp(..), someVar, Embeddable (..), varname)
import Test.IOTasks.ValueMap

import Z3.Monad
import Z3.Base (Goal)

import Test.QuickCheck (generate, vectorOf, elements)

import Control.Applicative (liftA2) -- not redundant for base-4.17.x.x and below!
import Control.Concurrent.STM

import Control.Monad
import Control.Monad.State

import Data.Maybe (catMaybes, fromMaybe, mapMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple.Extra (fst3, thd3)

import Data.List as List
import Test.QuickCheck.Gen (Gen)
import Type.Reflection
import Type.Match
import Data.List.Extra
import Control.Monad.Reader
import Data.Kind (Type)
import Data.Some (Some(Some))

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

type PrefixPath = [Some SimpleConstraint]
satPaths :: Int -> Int -> ConstraintTree -> Int -> Bool -> IO [SimplePath]
satPaths maxUnfolds to t maxSeqLength checkOverflows = do
  q <- atomically newTQueue
  nVar <- newTVarIO Nothing
  satPathsQ nVar to t maxUnfolds maxSeqLength checkOverflows q
  map fromJust . init <$> atomically (flushTQueue q)

satPathsQ :: TVar (Maybe Int) -> Int -> ConstraintTree -> Int -> Int -> Bool -> TQueue (Maybe SimplePath) -> IO ()
satPathsQ nVar to t maxUnfolds maxSeqLength checkOverflows q = do
  evalStateT (satPaths' 0 0 to t ([],ClosedPath [],0) q) NoContext
  atomically $ writeTQueue q Nothing
  where
    -- nUnfolds: number of Unfolds on current path
    -- nInputs: number of inputs on current path (path length)
    -- to: solver timeout
    -- (s,d,e): current prefix path, depth of current position in the tree (d =/= path length), current symbolic environment
    satPaths' :: Int -> Int -> Int -> ConstraintTree -> (PrefixPath,SimplePath,Int) -> TQueue (Maybe SimplePath) -> StateT SearchContext IO ()
    satPaths' _ _ to Empty (_,p,d) q = do
      let basePath = completePath [] p
      res <- lift $ isSatPath to basePath maxSeqLength checkOverflows
      modify $ updateContext res d
      when (res == SAT ()) $ lift $ atomically $ writeTQueue q $ Just p
    satPaths' nUnfolds nInputs to (Choice l r) (s,p,d) q = do
      satPaths' nUnfolds nInputs to l (s,p,d+1) q
      ctx <- get
      case ctx of
        RequirePruningCheck -> do
          let prefix = addEnvToPath $ reverse s
          res <- lift $ isSatPath to prefix maxSeqLength checkOverflows
          case res of
            NotSAT -> pure ()
            Timeout -> do
              put NoContext
              satPaths' nUnfolds nInputs to r (s,p,d+1) q
            SAT () -> do
              put NoContext
              satPaths' nUnfolds nInputs to r (s,p,d+1) q
        _ -> do
          satPaths' nUnfolds nInputs to r (s,p,d+1) q
    satPaths' nUnfolds nInputs to (Assert c@SimpleInput{} t) (s,p,d) q = do
      currentMaxPathLength <- lift $ readTVarIO nVar
      case currentMaxPathLength of
        Just len | nInputs >= len -> pure ()
        _ -> satPaths' nUnfolds (nInputs+1) to t (Some c:s,p `appendPath` ClosedPath [Some c],d+1) q -- stores constraints in reversed order
    satPaths' nUnfolds nInputs to (Assert c t) (s,p,d) q = satPaths' nUnfolds nInputs to t (Some c:s,p `appendPath` ClosedPath [Some c],d+1) q -- stores constraints in reversed order
    satPaths' nUnfolds nInputs to (Unfold t) (s,p,d) q
      | nUnfolds < maxUnfolds = satPaths' (nUnfolds+1) nInputs to t (s,p,d) q
      | otherwise = pure ()
    satPaths' nUnfolds nInputs to (InjectionPoint c t) (s,p,d) q = satPaths' nUnfolds nInputs to t (s,p `appendPath` OpenPath [] c (ClosedPath []),d) q

newtype PathInjector = PathInjector { injectNegatives :: Int -> Gen Path }

mkPathInjector :: SimplePath -> Timeout -> Int -> Bool -> IO PathInjector
mkPathInjector (ClosedPath cs) _ _ _ = pure . PathInjector . const . pure $ addEnvToPath cs
mkPathInjector p@OpenPath{} to maxSeqLength checkOverflows = do
  validPoints <- validInjectionPoints to p maxSeqLength checkOverflows
  pure $ PathInjector $ \n -> do
    is <- sort <$> vectorOf n (elements validPoints)
    pure $ completePath is p

validInjectionPoints :: Timeout -> SimplePath -> Int -> Bool -> IO [Int]
validInjectionPoints to p maxSeqLength checkOverflows = searchPoints [1..injectionPoints p]
  where
    searchPoints xs
      | n < 8 = fmap filterSATs $ forM xs $ \i -> (i,) <$> isSatPath to (completePath [i] p) maxSeqLength checkOverflows
      | otherwise = do
        res <- isSatPath to (completePath xs p) maxSeqLength checkOverflows
        case res of
          SAT () -> pure xs
          _ ->
            let (l,r) = splitAt (n `div` 2) xs
            in liftA2 (++) (searchPoints l) (searchPoints r)
      where n = length xs

    filterSATs :: [(Int,SatResult ())] -> [Int]
    filterSATs xs = [ i | (i,r) <- xs, r == SAT () ]


data ValueGenerator where
  ValueGenerator :: Typeable v => (ValueMap -> Size -> Gen v) -> ValueGenerator

withGeneratedValue :: (forall v. Typeable v => v -> r) -> ValueGenerator -> ValueMap -> Size -> IO r
withGeneratedValue f (ValueGenerator g) m sz = f <$> generate (g m sz)

pathScript :: Path -> ScriptMode -> Bool -> Z3R (SatResult [String], String)
pathScript path mode checkOverflows = do
  let (tyConstr,predConstr,overflowConstr) = partitionPath path

  goal <- mkGoal True False False
  vars <- forM tyConstr $
    \(InputConstraint (iVar@(Var (x,_)),i) (vs :: ValueSet v) e) -> do
      var <- mkFreshVar (x ++ show i) =<< mkSortFor iVar
      let r = maybe 0 (maximum . chronology) $ Map.lookup (someVar iVar) e
      pure (((someVar iVar,i),var,r),ValueGenerator $ \m -> valueOf iVar m vs)

  forM_ tyConstr $
    \(InputConstraint (iVar,i) (vs :: ValueSet v) e) -> do
      let varASTs = Map.fromListWith (++) $ concatMap (\(((var,j),ast,_),_) -> [ (var,[ast]) | j <= ix var e]) vars
      let var = fromMaybe (error "impossible") $ List.lookup (someVar iVar, i) $ map (dropThd . fst) vars
      constraint <- z3ValueSetConstraint iVar varASTs vs var
      goalAssert goal constraint
  forM_ predConstr $
    \(ConditionConstraint t e) ->
        goalAssert goal =<< z3Predicate goal t e (map (dropThd . fst) vars)

  when checkOverflows $
    forM_ overflowConstr $
      \(OverflowConstraints ts e) ->
        forM_ ts (\t -> assertOverflowChecks goal t e (map (dropThd . fst) vars))

  -- simplify and assert goal
  simplifyTactic <- mkTactic "simplify"
  tacticResult <- applyTactic simplifyTactic goal
  [newGoal] <- getApplyResultSubgoals tacticResult
  fs <- getGoalFormulas newGoal
  forM_ fs $ \f -> lift $ optimizeAssert f

  case mode of
    WithSoft -> do
      vs <- generateSuggestions vars
      forM_ vs $ \(ast,v) -> do
        cs <- mkValueRep ast v
        forM cs $ \(c,l) -> lift $ optimizeAssertSoft c "1" l -- soft assert with weight 1 and id "default"
    WithoutSoft -> pure ()
  str <- lift optimizeToString
  result <- lift $ optimizeCheck []
  mRes <- case result of
    Sat -> do
      model <- lift optimizeGetModel
      SAT . catMaybes <$> mapM (\(((var,_),ast,_),_) -> evalAST model (var,ast)) vars
    Unsat -> do
      pure NotSAT
    Undef -> pure Timeout
  pure (mRes,str)

dropThd :: (a,b,c) -> (a,b)
dropThd (x,y,_) = (x,y)

generateSuggestions :: [(((SomeVar, Int), AST, Int), ValueGenerator)] -> Z3R [(AST, Value)]
generateSuggestions vars = do
  -- make sure variables are ordered correctly, otherwise we construct the wrong ValueMap
  let
    sortedVars = sortOn (thd3 . fst) vars
    baseVars = map (fst . fst3 . fst) sortedVars
  ImplicitParameter {..} <- ask
  flip evalStateT (emptyValueMap baseVars) $
    forM vars $ \(((var,_),ast,_),gen) -> do
      vMap <- get
      (ast,val) <- liftIO $ withGeneratedValue (\v -> (ast,wrapValue v)) gen vMap (Size valueSizeParameter (maxSeqLengthParameter `div` 2))
      modify (insertValue val var)
      pure (ast,val)

evalAST :: MonadZ3 z3 => Model -> (SomeVar, AST) -> z3 (Maybe String)
evalAST m (SomeVar StringVar{},x) = do
  isS <- isStringSort =<< getSort x
  if isS
    then do
      mR <- modelEval m x True
      case mR of
        Just r -> Just <$> getString r
        Nothing -> pure Nothing
    else pure Nothing
evalAST m (SomeVar IntVar{},x) = fmap show <$> evalInt m x
evalAST m (SomeVar BoolVar{},x) = fmap show <$> evalBool m x
evalAST m (SomeVar (EmbeddedVar (_ :: TypeRep a) _),x) = fmap (show . asOriginal @a) <$> evalInt m x

mkSortFor :: MonadZ3 z3 => Var a -> z3 Sort
mkSortFor IntVar{} = mkIntSort
mkSortFor BoolVar{} = mkBoolSort
mkSortFor StringVar{} = mkStringSort
mkSortFor EmbeddedVar{} = mkIntSort

mkValueRep :: MonadZ3 z3 => AST -> Value -> z3 [(AST,Symbol)]
mkValueRep x (IntegerValue n) = do
  def <- mkStringSymbol "default"
  pure . (,def) <$> (mkEq x =<< mkInteger n)
mkValueRep x (BoolValue b) = do
  def <- mkStringSymbol "default"
  pure . (,def) <$> (mkEq x =<< mkBool b)
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


      -- binNoList (:&&:) = \a b -> mkAnd [a,b]
      -- binNoList (:||:) = \a b -> mkOr [a,b]

forStringElse :: forall (a :: Type) z3. (Typeable a, MonadZ3 z3) => (AST -> AST -> z3 AST) -> (AST -> AST -> z3 AST) -> AST -> AST -> z3 AST
forStringElse string normal = matchType @a
  [ inCaseOf' @String string
  , fallbackCase' normal
  ]

z3Predicate :: Typeable x => Goal -> Term 'Transparent x -> Map SomeVar SymbolicInfo -> [((SomeVar, Int), AST)] -> Z3R AST
z3Predicate goal (Add x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = \a b -> mkAdd [a,b]
}
z3Predicate goal (Sub x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = \a b -> mkSub [a,b]
}
z3Predicate goal (Mul x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = \a b -> mkMul [a,b]
}
z3Predicate goal (Equals x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = mkEq,
  binaryListAB = compareSymbolic (Strict EQ)
  }
z3Predicate goal (Gt (x :: Term k a) y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = forStringElse @a (\a b -> mkNot =<< mkStrLe a b) mkGt,
  binaryListAB = compareSymbolic (Strict GT)
  }
z3Predicate goal (Ge (x :: Term k a) y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = forStringElse @a (\a b -> mkNot =<< mkStrLt a b) mkGe,
  binaryListAB = compareSymbolic (ReflexiveClosure GT)
  }
z3Predicate goal (Lt (x :: Term k a) y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = forStringElse @a mkStrLt mkLt,
  binaryListAB = compareSymbolic (Strict LT)
  }
z3Predicate goal (Le (x :: Term k a) y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = forStringElse @a mkStrLe mkLe,
  binaryListAB = compareSymbolic (ReflexiveClosure LT)
  }
z3Predicate goal (And x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList = \a b -> mkAnd [a,b]
  }
z3Predicate goal (Or x y) e vars = z3PredicateBinary goal x y e vars $ binary {
  binaryNoList =  \a b -> mkOr [a,b]
  }
z3Predicate goal (IsIn x xs) e vars = z3PredicateBinary goal x xs e vars $ binary {
  binaryListB = \x xs -> mkOr =<< mapM (mkEq x) xs
  }
z3Predicate goal (Not x) e vars = z3PredicateUnary goal x e vars $ unary {
  unaryNoList = mkNot
  }
z3Predicate goal (Sum xs) e vars = z3PredicateUnary goal xs e vars $ unary {
  unaryList = mkAdd
  }
z3Predicate goal (Product xs) e vars = z3PredicateUnary goal xs e vars $ unary {
  unaryList = mkMul
  }
z3Predicate _ (Length (Current x n)) e vars = mkSeqLength . fromJust . (`List.lookup` vars) . last $ weaveVariables x n e --special case for string variables
z3Predicate goal (Length xs) e vars = z3PredicateUnary goal xs e vars $ unary {
  unaryList = mkIntNum . length
  }
z3Predicate goal (Reverse xs) e vars = z3PredicateUnary goal xs e vars $ unary {
  unaryList = error "z3Predicate: top level reverse should not happen"
  }
z3Predicate _ (IntLit n) _ _ = mkIntNum n
z3Predicate _ (ListLit _) _ _ = error "z3Predicate: top level list literal should not happen"
z3Predicate _ (EmbeddedLit x) _ _ = mkIntNum $ asInteger x
z3Predicate _ (BoolLit b) _ _ = mkBool b
z3Predicate _ (Current x n) e vars = pure $ fromMaybe (unknownVariablesError x) $ (`List.lookup` vars) . last $ weaveVariables x n e
z3Predicate _ All{} _ _ = error "z3Predicate: top level list should not happen"

data Un z3 = Un
  { unaryNoList :: AST -> z3 AST
  , unaryList :: [AST] -> z3 AST
  }
unary :: Un z3
unary = Un err err
  where err = error "does not happen with currently supported functions"

data Bin z3 = Bin
  { binaryNoList :: AST -> AST -> z3 AST
  , binaryListA :: [AST] -> AST -> z3 AST
  , binaryListB :: AST -> [AST] -> z3 AST
  , binaryListAB :: [AST] -> [AST] ->  z3 AST
  }
binary :: Bin z3
binary = Bin err err err err
  where err = error "does not happen with currently supported functions"

z3PredicateUnary :: forall a. Typeable a => Goal -> Term 'Transparent a ->  Map SomeVar SymbolicInfo -> [((SomeVar, Int), AST)] -> Un Z3R -> Z3R AST
z3PredicateUnary goal x e vars Un{..} = case typeRep @a of
  App c _ -> case eqTypeRep c (typeRep @[]) of
    Just HRefl -> case1 x -- a ~ [a1]
    Nothing -> case2 x -- a ~ f a1
  _ -> case2 x --
  where
    case1 :: forall a. Typeable [a] => Term 'Transparent [a] -> Z3R AST
    case1 x = do
      rx <- listASTs goal x e vars
      case rx of
        Right xs -> unaryList xs
        Left x -> unaryNoList x
    case2 :: forall a. Typeable a => Term 'Transparent a -> Z3R AST
    case2 x = unaryNoList =<< z3Predicate goal x e vars

z3PredicateBinary :: forall a b. (Typeable a, Typeable b) => Goal -> Term 'Transparent a -> Term 'Transparent b ->  Map SomeVar SymbolicInfo -> [((SomeVar, Int), AST)] -> Bin Z3R -> Z3R AST
z3PredicateBinary goal x y e vars Bin{..} = case typeRep @a of
  App ca _ -> case eqTypeRep ca (typeRep @[]) of
    Just HRefl -> case typeRep @b of
      App cb _ -> case eqTypeRep cb (typeRep @[]) of
        Just HRefl ->  case1 x y -- a ~ [a1], b ~ [b1]
        Nothing -> case2 x y -- a ~ [a1], b ~ f b1
      _ -> case2 x y -- a ~ [a1]
    Nothing -> case typeRep @b of
      App cb _ -> case eqTypeRep cb (typeRep @[]) of
        Just HRefl ->  case3 x y -- a ~ f a1, b ~ [b1]
        Nothing -> case4 x y -- a ~ f a1, b ~ f b1
      _ -> case4 x y-- a ~ f a1
  _ -> case typeRep @b of
    App cb _ -> case eqTypeRep cb (typeRep @[]) of
      Just HRefl ->  case3 x y -- b ~ [b1]
      Nothing -> case4 x y -- b ~ f b1
    _ -> case4 x y --
  where
  case1 :: forall a b. (Typeable [a], Typeable [b]) => Term 'Transparent [a] -> Term 'Transparent [b] -> Z3R AST
  case1 x y = do
    rx <- listASTs goal x e vars
    ry <- listASTs goal y e vars
    case (rx,ry) of
      (Right xs,Right ys) -> binaryListAB xs ys
      (Right xs,Left y) -> binaryListA xs y
      (Left x,Right ys) -> binaryListB x ys
      (Left x,Left y) -> binaryNoList x y
  case2 :: forall a b. (Typeable [a], Typeable b) => Term 'Transparent [a] -> Term 'Transparent b -> Z3R AST
  case2 x y = do
    exs <- listASTs goal x e vars
    y' <- z3Predicate goal y e vars
    case exs of
      Right xs -> binaryListA xs y'
      Left x -> binaryNoList x y'
  case3 :: forall a b. (Typeable a, Typeable [b]) => Term 'Transparent a -> Term 'Transparent [b] -> Z3R AST
  case3 x y = do
    x' <- z3Predicate goal x e vars
    eys <- listASTs goal y e vars
    case eys of
      Right ys -> binaryListB x' ys
      Left y -> binaryNoList x' y
  case4 :: forall a b. (Typeable a, Typeable b) => Term 'Transparent a -> Term 'Transparent b -> Z3R AST
  case4 x y = do
    xP <- z3Predicate goal x e vars
    yP <- z3Predicate goal y e vars
    binaryNoList xP yP

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

listASTs :: forall a. Typeable [a] => Goal -> Term 'Transparent [a] -> Map SomeVar SymbolicInfo -> [((SomeVar, Int), AST)] -> Z3R (Either AST [AST])
listASTs goal (Reverse (Reverse t)) e vars = listASTs goal t e vars
listASTs goal (Reverse t) e vars = do
  r <- listASTs goal t e vars
  case r of
    Right as -> pure $ Right $ reverse as
    Left a -> Left <$> reverseSequence goal a
listASTs _ (ListLit xs) _ _ =
  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Right <$> mapM (mkIntNum . toInteger) xs
    , inCaseOfE' @Char $ \HRefl -> Left <$> mkString xs
    , fallbackCase' $ error $ "list literal of unsupported type " ++ show (typeRep @[a])
    ]
listASTs _ (All x n) e vars = pure . Right $ lookupList (weaveVariables x n e) vars
listASTs _ (Current x n) e vars = pure . Left . head $ lookupList (weaveVariables x n e) vars
listASTs _ Add{} _ _ = error "lists should not have a Num instance"
listASTs _ Sub{} _ _ = error "lists should not have a Num instance"
listASTs _ Mul{} _ _ = error "lists should not have a Num instance"
listASTs _ Sum{} _ _ = error "lists should not have a Num instance"
listASTs _ Product{} _ _ = error "lists should not have a Num instance"

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

unknownVariablesError :: VarExp e => e a -> b
unknownVariablesError x = error $ "unknown variable(s) {" ++ intercalate "," (map varname $ toVarList x) ++ "}"

weaveVariables :: (VarExp e, Typeable a) => e a -> Int -> Map SomeVar SymbolicInfo -> [(SomeVar,Int)]
weaveVariables vs n e =
  reverse . drop n . reverse -- drop last n variables
  . map (\(x,y,_) -> (x,y))
  . sortOn thd3
  . concatMap (\(x,jks) -> map (\(j,k) -> (x,j,k)) jks)
  . mapMaybe ((\v -> ((v,) . storedValues) <$> Map.lookup v e) . someVar)
  $ toVarList vs

lookupList :: Eq a => [a] -> [(a, b)] -> [b]
lookupList vs vars = mapMaybe (`List.lookup` vars) vs

assertOverflowChecks :: Goal -> Term 'Transparent Integer ->  Map SomeVar SymbolicInfo -> [((SomeVar, Int), AST)] -> Z3R ()
assertOverflowChecks goal t e vars = do
  ast <- z3Predicate goal t e vars
  overflowConstraints goal ast

overflowConstraints :: Goal -> AST -> Z3R ()
overflowConstraints goal x = do
  minInt <- mkInteger (toInteger $ minBound @Int)
  maxInt <- mkInteger (toInteger $ maxBound @Int)
  lift (goalAssert goal =<< mkGe x minInt)
  lift (goalAssert goal =<< mkLe x maxInt)
