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

import Test.QuickCheck.Gen (Gen)
import Type.Reflection
import Data.Either (fromRight)
import Type.Match

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
      forM_ vs $ \(ast,v) -> do
        cs <- mkValueRep ast v
        forM cs $ \(c,l) -> optimizeAssertSoft c "1" l -- soft assert with weight 1 and id "default"
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
  case eqTypeRep (typeRep @a) (typeRep @Integer) of
    Just HRefl -> mkIntSort
    Nothing -> case eqTypeRep (typeRep @a) (typeRep @String) of
      Just HRefl -> mkStringSort
      Nothing -> error "mkSort: unsupported type"

mkValueRep :: AST -> Value -> Z3 [(AST,Symbol)]
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

z3Predicate :: Term x -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
z3Predicate (termStruct -> Binary f x y) e vars = z3PredicateBinary f x y e vars
z3Predicate (termStruct -> Unary f x) e vars = z3PredicateUnary f x e vars
z3Predicate (termStruct -> Variable C x n) e vars = pure $ fromMaybe (unknownVariablesError x) $ (`lookup` vars) . last $ weaveVariables x n e
z3Predicate (termStruct -> Literal (IntLit n)) _ _ = mkIntNum n
z3Predicate (termStruct -> Literal (BoolLit b)) _ _ = mkBool b
--
z3Predicate (termStruct -> Variable A _x _) _e _vars = error "z3Predicate: top level list should not happen"
z3Predicate (termStruct -> Literal (ListLit _)) _ _ = error "z3Predicate: top level list literal should not happen"

z3PredicateUnary :: forall a b. Typeable a => UnaryF a b -> Term a ->  Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
z3PredicateUnary f x e vars = case typeRep @a of
  App c _ -> case eqTypeRep c (typeRep @[]) of
    Just HRefl -> unaryListA f x e vars  -- a ~ [a1]
    Nothing -> unaryNoList f x e vars-- a ~ f a1
  _ -> unaryNoList f x e vars --

unaryListA :: UnaryF [a] b -> Term [a] -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
unaryListA Length (Current x n) e vars = mkSeqLength . fromJust . (`lookup` vars) . last $ weaveVariables x n e --special case for string variables
unaryListA Length xs e vars = unaryListRec (mkIntNum . length @[]) xs e vars
unaryListA Reverse _ _ _ = error "z3Predicate: top level reverse should not happen"
unaryListA Sum xs e vars = unaryListRec mkAdd xs e vars
unaryListA Product xs e vars = unaryListRec mkMul xs e vars

unaryNoList :: UnaryF a b -> Term a -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
unaryNoList Not x e vars = mkNot =<< z3Predicate x e vars
unaryNoList _ _ _ _ = error "handled by unaryListA"

unaryListRec :: Typeable a => ([AST] -> Z3 AST) -> Term [a] -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
unaryListRec f xs e vars = f . fromRight (error "unexpected resutl") =<< listASTs xs e vars

z3PredicateBinary :: forall a b c. (Typeable a, Typeable b) => BinaryF a b c -> Term a -> Term b ->  Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 AST
z3PredicateBinary f x y e vars = case typeRep @a of
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
  case1 :: forall a b c. (Typeable [a], Typeable [b]) => BinaryF [a] [b] c -> Term [a] -> Term [b] -> Z3 AST
  case1 f x y = do
    rx <- listASTs x e vars
    ry <- listASTs y e vars
    case (rx,ry) of
      (Right xs,Right ys) -> binListAB f xs ys
      (Right xs,Left y) -> binListA f xs y
      (Left x,Right ys) -> binListB f x ys
      (Left x,Left y) -> binNoList f x y
  case2 :: forall a b c. Typeable [a] => BinaryF [a] b c -> Term [a] -> Term b -> Z3 AST
  case2 f x y = do
    exs <- listASTs x e vars
    y' <- z3Predicate y e vars
    case exs of
      Right xs -> binListA f xs y'
      Left x -> binNoList f x y'
  case3 :: forall a b c. Typeable [b] => BinaryF a [b] c -> Term a -> Term [b] -> Z3 AST
  case3 f x y = do
    x' <- z3Predicate x e vars
    eys <- listASTs y e vars
    case eys of
      Right ys -> binListB f x' ys
      Left y -> binNoList f x' y
  case4 :: forall a b c. BinaryF a b c -> Term a -> Term b -> Z3 AST
  case4 f x y = do
    xP <- z3Predicate x e vars
    yP <- z3Predicate y e vars
    binNoList f xP yP

binNoList :: BinaryF a b c -> AST -> AST -> Z3 AST
binNoList (:+:) = \a b -> mkAdd [a,b]
binNoList (:-:) = \a b -> mkSub [a,b]
binNoList (:*:) = \a b -> mkMul [a,b]
binNoList (:==:) = mkEq
binNoList (:>:) = mkGt
binNoList (:>=:) = mkGe
binNoList (:<:) = mkLt
binNoList (:<=:) = mkLe
binNoList (:&&:) = \a b -> mkAnd [a,b]
binNoList (:||:) = \a b -> mkOr [a,b]
binNoList IsIn = error "handled by binListB"

binListA :: BinaryF [a] b c -> [AST] -> AST -> Z3 AST
binListA = error "does not happen with currently supported functions"

binListB :: BinaryF a [b] c -> AST -> [AST] -> Z3 AST
binListB IsIn x ys = mkOr =<< mapM (mkEq x) ys
binListB _ _ _ = error "all other functions are handled by binListAB"

binListAB :: BinaryF [a] [b] c -> [AST] -> [AST] -> Z3 AST
binListAB (:==:) = compareSymbolic (mkEq,(==))
binListAB (:>:)  = compareSymbolic (mkGt,(>))
binListAB (:>=:) = compareSymbolic (mkGe,(>=))
binListAB (:<:) = compareSymbolic (mkLt,(<))
binListAB (:<=:) = compareSymbolic (mkLe,(<=))

compareSymbolic :: (AST -> AST -> Z3 AST, Int -> Int -> Bool) -> [AST] -> [AST] -> Z3 AST
compareSymbolic (f,g) xs ys
  | length xs == length ys = mkAnd =<< zipWithM f xs ys
  | otherwise = mkBool $ g (length xs) (length ys)

listASTs :: forall a. Typeable [a] => Term [a] -> Map Var (Int,[Int]) -> [((Var, Int), AST)] -> Z3 (Either AST [AST])
listASTs (ReverseT t) e vars = do
  r <- listASTs t e vars
  case r of
    Right as -> pure $ Right $ reverse as
    Left a -> Left <$> reverseSequence a
listASTs (ListLitT xs) _ _ =
  matchType @a
    [ inCaseOfE' @Integer $ \HRefl -> Right <$> mapM (mkIntNum . toInteger) xs
    , inCaseOfE' @Char $ \HRefl -> Left <$> mkString xs
    , fallbackCase' $ error $ "list literal of usupported type " ++ show (typeRep @[a])
    ]
listASTs (All x n) e vars = pure . Right $ lookupList (weaveVariables x n e) vars
listASTs (Current x n) e vars = pure . Left . head $ lookupList (weaveVariables x n e) vars

reverseSequence :: AST -> Z3 AST
reverseSequence x = do
  y <- mkFreshVar "reversed" =<< getSort x
  lx <- mkSeqLength x
  ly <- mkSeqLength y
  optimizeAssert =<< mkEq lx ly
  forM_ [0 :: Int .. 20] (optimizeAssert <=< pos (x,lx) (y,ly))
  pure y
  where
    pos (x,lx) (y,ly) i = do
      index <- mkIntNum i
      pre <- mkGe lx index
      xi <- mkSeqAt x index
      index' <- mkIntNum (i+1)
      yj <- mkSeqAt y =<< mkSub [ly,index']
      con <- mkEq xi yj
      mkImplies pre con

unknownVariablesError :: VarExp a => a -> b
unknownVariablesError x = error $ "unknown variable(s) {" ++ intercalate "," (map varname $ toVarList x) ++ "}"

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
