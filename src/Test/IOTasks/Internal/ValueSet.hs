{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Test.IOTasks.Internal.ValueSet (
  ValueSet(..),
  empty, complete,
  singleton, fromList,
  singletonStr, fromListStr,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  withStr, withoutStr,
  complement,
  unique, notYetIn, alreadyIn, differentFromLast, repeatLast,
  embed, embedFromList,
  isEmpty,
  containsValue, initiallyContainsValue,
  showValueSet,
  valueOf,
  Size(..),
  ints, nats, bools, str,
  z3ValueSetConstraint,
  ) where

import Data.Set (Set)
import qualified Data.Set as Set (union,intersection,filter,toList,fromAscList, empty, fromList)
import Type.Reflection

import Test.QuickCheck

import Z3.Monad
import Test.IOTasks.ValueMap (ValueMap, emptyValueMap, sortedEntries, ValueEntry (IntegerEntry, NoEntry, BoolEntry, StringEntry),  traverseValueEntries)
import Test.IOTasks.Var (Var (..), SomeVar (..), someVar, intVar, Embedded (Embedded), Embeddable (..), Varname, varname, VarExp (toVarList))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Extra (maximumOn)
import Data.Functor ((<&>))

data ValueSet a where
  Union :: ValueSet a -> ValueSet a -> ValueSet a
  Intersection :: ValueSet a -> ValueSet a -> ValueSet a
  GreaterThan :: Integer -> ValueSet Integer
  LessThan :: Integer -> ValueSet Integer
  EqInt :: Integer -> ValueSet Integer
  CurrentlyIn :: VarRef a -> ValueSet a
  CurrentlyNotIn :: VarRef a -> ValueSet a
  DifferentFromLast :: VarRef a -> ValueSet a
  SameAsLast :: VarRef a -> ValueSet a
  EqStr :: String -> ValueSet String
  NotStr :: String -> ValueSet String

  Every :: ValueSet a
  None :: ValueSet a
  Embed :: Embeddable a => ValueSet Integer -> ValueSet (Embedded a)

deriving instance Eq a => Eq (ValueSet a)
deriving instance Ord a => Ord (ValueSet a)

data VarRef a = Self | Other (NonEmpty (Var a))
  deriving (Eq,Ord)

hasVarRefs :: ValueSet a -> Bool
hasVarRefs Every = False
hasVarRefs None = False
hasVarRefs GreaterThan{} = False
hasVarRefs LessThan{} = False
hasVarRefs EqInt{} = False
hasVarRefs EqStr{} = False
hasVarRefs NotStr{} = False
hasVarRefs (Embed vs) = hasVarRefs vs
hasVarRefs CurrentlyIn{} = True
hasVarRefs CurrentlyNotIn{} = True
hasVarRefs DifferentFromLast{} = True
hasVarRefs SameAsLast{} = True
hasVarRefs (Union x y) = hasVarRefs x || hasVarRefs y
hasVarRefs (Intersection x y) = hasVarRefs x || hasVarRefs y

data Size = Size { intAbs :: Integer, strLen :: Int }

empty :: ValueSet a
empty = None

complete :: ValueSet a
complete = Every

singleton :: Integer -> ValueSet Integer
singleton = EqInt

singletonStr :: String -> ValueSet String
singletonStr = EqStr

fromList :: [Integer] -> ValueSet Integer
fromList = foldr (union . intervalConstraint) empty . optimizeList . sort
  where
    intervalConstraint :: Either Integer (Integer,Integer) -> ValueSet Integer
    intervalConstraint (Left i) = EqInt i
    intervalConstraint (Right (i,j)) = GreaterThan (i-1) `intersection` LessThan (j+1)

-- ASSUMPTION: xs is sorted
optimizeList :: [Integer] -> [Either Integer (Integer,Integer)]
optimizeList = go Nothing where
  go :: Maybe (Integer,Maybe Integer) -> [Integer] -> [Either Integer (Integer,Integer)]
  go Nothing [] = []
  go (Just (i, Nothing)) [] = [Left i]
  go (Just (i, Just j)) [] = [Right (i,j)]
  go Nothing (x:xs) = go (Just (x, Nothing)) xs
  go (Just (i, Nothing)) (x:xs)
    | x == i+1 = go (Just (i,Just x)) xs
    | otherwise = Left i : go (Just (x,Nothing)) xs
  go (Just (i, Just j)) (x:xs)
    | x == j+1 = go (Just (i,Just x)) xs
    | otherwise = Right (i,j) : go (Just (x,Nothing)) xs

fromListStr :: [String] -> ValueSet String
fromListStr = foldr (flip withStr) empty

unique :: ValueSet a -> ValueSet a
unique vs = CurrentlyNotIn Self `intersection` vs

notYetIn :: VarExp e => ValueSet a -> e a -> ValueSet a
notYetIn vs e = CurrentlyNotIn (Other $ NonEmpty.fromList $ toVarList e) `intersection` vs

alreadyIn :: VarExp e => e a -> ValueSet a
alreadyIn e = CurrentlyIn (Other $ NonEmpty.fromList $ toVarList e)

differentFromLast :: VarExp e => ValueSet a -> e a -> ValueSet a
differentFromLast vs e = DifferentFromLast (Other $ NonEmpty.fromList $ toVarList e) `intersection` vs

repeatLast :: VarExp e => e a -> ValueSet a
repeatLast e = SameAsLast (Other $ NonEmpty.fromList $ toVarList e)

greaterThan :: Integer -> ValueSet Integer
greaterThan = GreaterThan

lessThan :: Integer -> ValueSet Integer
lessThan = LessThan

union :: ValueSet a -> ValueSet a -> ValueSet a
union = Union

intersection :: ValueSet a -> ValueSet a -> ValueSet a
intersection = Intersection

-- | The '(\\)' operator computes set difference.
-- It returns a 'ValueSet' containing all integers of the first set that are not present in the second set.
(\\) :: ValueSet a -> ValueSet a -> ValueSet a
(\\) xs = (xs `intersection`) . complement

without :: ValueSet Integer -> Integer -> ValueSet Integer
without vs = (vs \\) . singleton

with :: ValueSet Integer -> Integer -> ValueSet Integer
with vs = (vs `union`) . singleton

withoutStr :: ValueSet String -> String -> ValueSet String
withoutStr vs = (vs \\) . singletonStr

withStr :: ValueSet String -> String -> ValueSet String
withStr vs = (vs `union`) . singletonStr

embed ::  Embeddable a => ValueSet Integer -> ValueSet (Embedded a)
embed vs
  | hasVarRefs vs = error "embed: cannot embed value sets with references to variables"
  | otherwise = Embed vs

embedFromList :: Embeddable a => [a] -> ValueSet (Embedded a)
embedFromList = embed . fromList . map asInteger

complement :: ValueSet a -> ValueSet a
complement (GreaterThan n) = LessThan n `union` EqInt n
complement (LessThan n) =  GreaterThan n `union` EqInt n
complement (Intersection va vb) = Union (complement va) (complement vb)
complement (Union va vb) = Intersection (complement va) (complement vb)
complement (EqInt n) = GreaterThan n `union` LessThan n
complement (EqStr str) = NotStr str
complement (NotStr str) = EqStr str
complement (CurrentlyIn x) = CurrentlyNotIn x
complement (CurrentlyNotIn x) = CurrentlyIn x
complement (DifferentFromLast x) = SameAsLast x
complement (SameAsLast x) = DifferentFromLast x
complement Every = None
complement None = Every
complement (Embed vs) = embedComplement (Embed vs)
  where
    embedComplement :: forall a. Embeddable a => ValueSet (Embedded a) -> ValueSet (Embedded a)
    embedComplement (Embed vs) = Embed (fromList (embeddingRange @a) \\ vs)
    embedComplement x = complement x

containsValue :: Var a -> ValueMap -> ValueSet a -> a -> Bool
containsValue x m (Union vs1 vs2) n = containsValue x m vs1  n || containsValue x m vs2 n
containsValue x m (Intersection vs1 vs2) n = containsValue x m vs1 n && containsValue x m vs2 n
containsValue _ _ (GreaterThan i) n = n > i
containsValue _ _ (LessThan i) n = n < i
containsValue _ _ (EqInt i) n = i == n
containsValue _ _ (EqStr str) x = str == x
containsValue _ _ (NotStr str) x = str /= x
containsValue x m (CurrentlyIn Self) n = withVarEq x $ n `elem` valuesInVar x m
containsValue _ m (CurrentlyIn (Other ys)) n = withVarEq (NonEmpty.head ys) $ n `elem` valuesInVars ys m
containsValue x m (CurrentlyNotIn Self) n = withVarEq x $ n `notElem` valuesInVar x m
containsValue _ m (CurrentlyNotIn (Other ys)) n = withVarEq (NonEmpty.head ys) $  n `notElem` valuesInVars ys m
containsValue x m (DifferentFromLast Self) n = withVarEq x $ Just n /= lastValueOfVar x m
containsValue _ m (DifferentFromLast (Other ys)) n = withVarEq (NonEmpty.head ys) $ Just n /= lastValueOfVars ys m
containsValue x m (SameAsLast Self) n = withVarEq x $ Just n == lastValueOfVar x m
containsValue _ m (SameAsLast (Other ys)) n = withVarEq (NonEmpty.head ys) $ Just n == lastValueOfVars ys m
containsValue _ _ Every _ = True
containsValue _ _ None _ = False
containsValue x m (Embed vs) v = containsEmbeddedValue x m vs v

withVarEq :: Var a -> (Eq a => r) -> r
withVarEq IntVar{} x = x
withVarEq BoolVar{} x = x
withVarEq StringVar{} x = x
withVarEq EmbeddedVar{} x = x

containsEmbeddedValue :: Var (Embedded a) -> ValueMap -> ValueSet Integer -> Embedded a -> Bool
containsEmbeddedValue _ _ Every _ = True
containsEmbeddedValue _ _ None _ = False
containsEmbeddedValue x m (Union a b) v = containsEmbeddedValue x m a v || containsEmbeddedValue x m b v
containsEmbeddedValue x m (Intersection a b) v = containsEmbeddedValue x m a v && containsEmbeddedValue x m b v
containsEmbeddedValue _ _ (GreaterThan i) (Embedded n) = n > i
containsEmbeddedValue _ _ (LessThan i) (Embedded n) = n < i
containsEmbeddedValue _ _ (EqInt i) (Embedded n) = i == n
containsEmbeddedValue x m (CurrentlyIn Self) n = withVarEq x $ n `elem` valuesInVar x m
containsEmbeddedValue _ m (CurrentlyIn (Other ys)) (Embedded n) = withVarEq (NonEmpty.head ys) $ n `elem` valuesInVars ys m
containsEmbeddedValue x m (CurrentlyNotIn Self) n = withVarEq x $ n `notElem` valuesInVar x m
containsEmbeddedValue _ m (CurrentlyNotIn (Other ys)) (Embedded n) = withVarEq (NonEmpty.head ys) $ n `notElem` valuesInVars ys m
containsEmbeddedValue x m (SameAsLast Self) n = withVarEq x $ Just n == lastValueOfVar x m
containsEmbeddedValue _ m (SameAsLast (Other ys)) (Embedded n) = withVarEq (NonEmpty.head ys) $ Just n == lastValueOfVars ys m
containsEmbeddedValue x m (DifferentFromLast Self) n = withVarEq x $ Just n /= lastValueOfVar x m
containsEmbeddedValue _ m (DifferentFromLast (Other ys)) (Embedded n) = withVarEq (NonEmpty.head ys) $ Just n /= lastValueOfVars ys m

lastValueOfVar :: Var a -> ValueMap -> Maybe a
lastValueOfVar x m = case valuesInVar x m of
  [] -> Nothing
  xs -> Just $ last xs

lastValueOfVars :: NonEmpty (Var a) -> ValueMap -> Maybe a
lastValueOfVars x m = case valuesInVars x m of
  [] -> Nothing
  xs -> Just $ last xs

valuesInVar :: Var a -> ValueMap -> [a]
valuesInVar = valuesInVars . NonEmpty.singleton

valuesInVars :: NonEmpty (Var a) -> ValueMap -> [a]
valuesInVars xs@(IntVar{} :| _) env =
  case sortedEntries (NonEmpty.toList xs) env of
    IntegerEntry values -> map fst values
    NoEntry -> []
    _ -> error "valuesIn: impossible (type error)"
valuesInVars xs@(EmbeddedVar ty _ :| _) env =
  case withTypeable ty $ sortedEntries (NonEmpty.toList xs) env of
    IntegerEntry values -> map (Embedded . fst) values
    NoEntry -> []
    _ -> error "valuesIn: impossible (type error)"
valuesInVars xs@(BoolVar{} :| _) env =
  case sortedEntries (NonEmpty.toList xs) env of
    BoolEntry values -> map fst values
    NoEntry -> []
    _ -> error "valuesIn: impossible (type error)"
valuesInVars xs@(StringVar{} :| _) env =
  case sortedEntries (NonEmpty.toList xs) env of
    StringEntry values -> map fst values
    NoEntry -> []
    _ -> error "valuesIn: impossible (type error)"


initiallyContainsValue ::  ValueSet Integer -> Integer -> Bool
initiallyContainsValue = containsValue (intVar "x") $ emptyValueMap [someVar $ intVar "x"]

valueOf :: Var a -> ValueMap -> ValueSet a -> Size -> Gen a
valueOf _ _ None = error "valueOf: cannot draw value from empty value set"
valueOf x@IntVar{} m vs = valueOfInt x m vs
valueOf x@BoolVar{} m vs = const $ valueOfBool x m vs
valueOf x@StringVar{} m vs = valueOfString x m vs
valueOf x@EmbeddedVar{} m vs = valueOfEmbedded x m vs

valueOfInt :: Var Integer -> ValueMap -> ValueSet Integer -> Size -> Gen Integer
valueOfInt (IntVar x) = valueOfIntOrEmbedded id IntVar (\(Size sz _) -> [-sz..sz]) x

valueOfEmbedded :: Var (Embedded a) -> ValueMap -> ValueSet (Embedded a) -> Size -> Gen (Embedded a)
valueOfEmbedded (EmbeddedVar (ty :: TypeRep x) x) = withTypeable ty $ valueOfIntOrEmbedded Embedded (EmbeddedVar ty) (const $ embeddingRange @x) x

valueOfIntOrEmbedded :: (Typeable a, Eq a) => (Integer -> a) -> (Varname -> Var a) -> (Size -> [Integer]) -> Varname -> ValueMap -> ValueSet a -> Size -> Gen a
valueOfIntOrEmbedded f g h x m vs sz =
  case Set.toList $ range vs $ Set.fromAscList (h sz) of
    [] -> error $ unwords ["valueOf: no values within size limits for",showValueSet vs]
    xs -> f <$> elements xs
  where
    range :: ValueSet a -> Set Integer -> Set Integer
    range (Union x y) r = range x r `Set.union` range y r
    range (Intersection x y) r = range x r `Set.intersection` range y r
    range (GreaterThan n) r = Set.filter (>n) r
    range (LessThan n) r = Set.filter (<n) r
    range (EqInt n) r = Set.filter (==n) r
    range (EqStr _) _ = error "valueOfIntOrEmbedded: should not happen"
    range (NotStr _) _ = error "valueOfIntOrEmbedded: should not happen"
    range (CurrentlyIn Self) r = Set.filter (\v -> f v `elem` valuesInVar (g x) m) r
    range (CurrentlyIn (Other ys)) r = Set.filter (\v -> f v `elem` valuesInVars (NonEmpty.map (g . varname) ys) m) r
    range (CurrentlyNotIn Self) r = Set.filter (\v -> f v `notElem` valuesInVar (g x) m) r
    range (CurrentlyNotIn (Other ys)) r = Set.filter (\v -> f v `notElem` valuesInVars (NonEmpty.map (g . varname) ys) m) r
    range (SameAsLast Self) r = Set.filter (\v -> Just (f v) == lastValueOfVar (g x) m) r
    range (SameAsLast (Other ys)) r = Set.filter (\v -> Just (f v) == lastValueOfVars (NonEmpty.map (g . varname) ys) m) r
    range (DifferentFromLast Self) r = Set.filter (\v -> Just (f v) /= lastValueOfVar (g x) m) r
    range (DifferentFromLast (Other ys)) r = Set.filter (\v -> Just (f v) /= lastValueOfVars (NonEmpty.map (g . varname) ys) m) r
    range Every r = r
    range None _ = Set.empty
    range (Embed vs) r = range vs r

valueOfBool :: Var Bool -> ValueMap -> ValueSet Bool -> Gen Bool
valueOfBool x m vs =
  case Set.toList $ range vs $ Set.fromList [True,False] of
    [] -> error $ unwords ["valueOf: no boolean values between for",showValueSet vs]
    xs -> elements xs
  where
    range :: ValueSet Bool -> Set Bool -> Set Bool
    range (Union x y) r = range x r `Set.union` range y r
    range (Intersection x y) r = range x r `Set.intersection` range y r
    range (CurrentlyIn Self) r = Set.filter (\v -> v `elem` valuesInVar x m) r
    range (CurrentlyIn (Other ys)) r = Set.filter (\v -> v `elem` valuesInVars ys m) r
    range (CurrentlyNotIn Self) r = Set.filter (\v -> v `notElem` valuesInVar x m) r
    range (CurrentlyNotIn (Other ys)) r = Set.filter (\v -> v `notElem` valuesInVars ys m) r
    range (SameAsLast Self) r = Set.filter (\v -> Just v == lastValueOfVar x m) r
    range (SameAsLast (Other ys)) r = Set.filter (\v -> Just v == lastValueOfVars ys m) r
    range (DifferentFromLast Self) r = Set.filter (\v -> Just v /= lastValueOfVar x m) r
    range (DifferentFromLast (Other ys)) r = Set.filter (\v -> Just v /= lastValueOfVars ys m) r
    range Every r = r
    range None _ = Set.empty

valueOfString :: Var String -> ValueMap -> ValueSet String -> Size -> Gen String
valueOfString x m vs (Size _ len) =
  case reduceStringSet x m vs of
    FromThese [] -> error $ unwords ["valueOf: no string values for", showValueSet vs]
    FromThese xs -> elements $ nub xs
    NotThese xs -> resize len (listOf $ elements ['a'..'z']) `suchThat` (`notElem` xs)

data Str = FromThese [String] | NotThese [String]

reduceStringSet :: Var String -> ValueMap -> ValueSet String -> Str
reduceStringSet _ _ Every = NotThese []
reduceStringSet _ _ None = FromThese []
reduceStringSet _ _ (EqStr str) = FromThese [str]
reduceStringSet _ _ (NotStr str) = NotThese [str]
reduceStringSet x m (CurrentlyIn Self) = FromThese (valuesInVar x m)
reduceStringSet _ m (CurrentlyIn (Other ys)) = FromThese (valuesInVars ys m)
reduceStringSet x m (CurrentlyNotIn Self) = NotThese (valuesInVar x m)
reduceStringSet _ m (CurrentlyNotIn (Other ys)) = NotThese (valuesInVars ys m)
reduceStringSet x m (SameAsLast Self) = FromThese $ maybeToList $ lastValueOfVar x m
reduceStringSet _ m (SameAsLast (Other ys)) = FromThese $ maybeToList $ lastValueOfVars ys m
reduceStringSet x m (DifferentFromLast Self) = NotThese $ maybeToList $ lastValueOfVar x m
reduceStringSet _ m (DifferentFromLast (Other ys)) = NotThese $ maybeToList $ lastValueOfVars ys m
reduceStringSet x m (Union a b) = case (reduceStringSet x m a, reduceStringSet x m b) of
  (FromThese xs, FromThese ys) -> FromThese (xs ++ ys)
  (FromThese xs, NotThese ys) -> NotThese [ y | y <- ys, y `notElem` xs ]
  (NotThese xs, FromThese ys) -> NotThese [ x | x <- xs, x `notElem` ys ]
  (NotThese xs, NotThese ys) -> NotThese [ x | x <- xs, x `elem` ys ]
reduceStringSet x m (Intersection a b) = case (reduceStringSet x m a, reduceStringSet x m b) of
  (FromThese xs, FromThese ys) -> FromThese [ x | x <- xs, x `elem` ys]
  (FromThese xs, NotThese ys) -> FromThese  [ x | x <- xs, x `notElem` ys ]
  (NotThese xs, FromThese ys) -> FromThese [ y | y <- ys, y `notElem` xs ]
  (NotThese xs, NotThese ys) -> NotThese (xs ++ ys)

showValueSet :: Typeable a => ValueSet a -> String
showValueSet = go where
  go :: forall a. Typeable a => ValueSet a -> String
  go vs = concat ["{ v : ",show (typeRep @a), " | ", showValueSet' vs ,"}"]

  showValueSet' :: ValueSet a -> String
  showValueSet' (Union vs1 vs2) = concat ["(",showValueSet' vs1,") \\/ (", showValueSet' vs2,")"]
  showValueSet' (Intersection vs1 vs2) = concat ["(",showValueSet' vs1,") /\\ (", showValueSet' vs2,")"]
  showValueSet' (GreaterThan n) = "v > " ++ show n
  showValueSet' (LessThan n) = "v < " ++ show n
  showValueSet' (EqInt n) = "v == " ++ show n
  showValueSet' (EqStr str) = "v == " ++ show str
  showValueSet' (NotStr str) = "v /= " ++ show str
  showValueSet' (CurrentlyIn ref) = "currentlyIn " ++ showVarRef ref
  showValueSet' (CurrentlyNotIn ref) = "currentlyNotIn " ++ showVarRef ref
  showValueSet' (SameAsLast ref) = "sameAsLast " ++ showVarRef ref
  showValueSet' (DifferentFromLast ref) = "differentFromLast " ++ showVarRef ref
  showValueSet' Every = "true"
  showValueSet' None = "false"
  showValueSet' (Embed vs) = "Embed " ++ showValueSet' vs

  showVarRef Self = "self"
  showVarRef (Other y) = show y

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = EqInt 0 `Union` GreaterThan 0

bools :: ValueSet Bool
bools = complete

str :: ValueSet String
str = Every

--
-- | Check if a given 'ValueSet' of integers is empty.
--
-- This function uses an external SMT solver to check the constraints defined by the 'ValueSet'.
isEmpty :: Var Integer -> ValueMap -> ValueSet Integer -> IO Bool
isEmpty var m vs = evalZ3 $ do
  x <- mkIntVar =<< mkStringSymbol "x"
  astMap <- mkASTMap
  assert =<< z3ValueSetConstraint var astMap vs x
  res <- check
  pure $ case res of
    Sat -> False
    Unsat -> True
    Undef -> error "isEmpty: could not solve SMT constraints"
  where
    mkASTMap :: Z3 (Map SomeVar [(AST,Int)])
    mkASTMap = traverseValueEntries m $ \case
      BoolEntry xs -> mapM (\(b,i) -> mkBool b <&> (,i)) xs
      IntegerEntry xs -> mapM (\(b,i) -> mkInteger b <&> (,i)) xs
      StringEntry xs -> mapM (\(b,i) -> mkString b <&> (,i)) xs
      NoEntry -> pure []


z3ValueSetConstraint :: MonadZ3 z3 => Typeable a => Var a -> Map SomeVar [(AST,Int)] -> ValueSet a -> AST -> z3 AST
z3ValueSetConstraint var m (Union x y) xVar = do
  cx <- z3ValueSetConstraint var m x xVar
  cy <- z3ValueSetConstraint var m y xVar
  mkOr [cx,cy]
z3ValueSetConstraint var m (Intersection x y) xVar = do
  cx <- z3ValueSetConstraint var m x xVar
  cy <- z3ValueSetConstraint var m y xVar
  mkAnd [cx,cy]
z3ValueSetConstraint _ _ (GreaterThan n) xVar = mkIntNum n >>= mkGt xVar
z3ValueSetConstraint _ _ (LessThan n) xVar = mkIntNum n >>= mkLt xVar
z3ValueSetConstraint _ _ (EqInt n) xVar = mkIntNum n >>= mkEq xVar
z3ValueSetConstraint _ _ (EqStr str) xVar = mkString str >>= mkEq xVar
z3ValueSetConstraint _ _ (NotStr str) xVar = mkString str >>= mkEq xVar >>= mkNot
z3ValueSetConstraint var m (CurrentlyIn Self) xVar = do
  let xs = maybe [] (map fst) (Map.lookup (someVar var) m)
  mkOr =<< sequence [ mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint _ m (CurrentlyIn (Other ys)) xVar = do
  let xs = maybe [] (map fst . concat . NonEmpty.toList) $ traverse ((`Map.lookup` m) . someVar) ys
  mkOr =<< sequence [ mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint var m (CurrentlyNotIn Self) xVar = do
  let xs = maybe [] (map fst) $ Map.lookup (someVar var) m
  mkAnd =<< sequence [ mkNot =<< mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint _ m (CurrentlyNotIn (Other ys)) xVar = do
  let xs = maybe [] (map fst . concat . NonEmpty.toList) $ traverse ((`Map.lookup` m) . someVar) ys
  mkAnd =<< sequence [ mkNot =<< mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint var m (SameAsLast Self) xVar =
  case lookupLastAST (someVar var) m of
    Just x -> mkEq xVar x
    Nothing -> error $ "no values for " ++ show var ++ " found! (you are probably specifying inputs to be from an empty set)"
z3ValueSetConstraint _ m (SameAsLast (Other ys)) xVar =
  case lookupLastsAST (NonEmpty.map someVar ys) m of
    Just x -> mkNot =<< mkEq xVar x
    Nothing -> error $ "no values for " ++ show ys ++ " found! (you are probably specifying inputs to be from an empty set)"
z3ValueSetConstraint var m (DifferentFromLast Self) xVar =
  case lookupLastAST (someVar var) m of
    Just x -> mkNot =<< mkEq xVar x
    Nothing -> mkTrue
z3ValueSetConstraint _ m (DifferentFromLast (Other ys)) xVar =
  case lookupLastsAST (NonEmpty.map someVar ys) m of
    Just x -> mkNot =<< mkEq xVar x
    Nothing -> mkTrue
z3ValueSetConstraint x@(EmbeddedVar (_ :: TypeRep x) _) m Every xVar = z3ValueSetConstraint x m (Embed $ fromList (embeddingRange @x)) xVar
z3ValueSetConstraint _ _ Every _ = mkTrue
z3ValueSetConstraint _ _ None _ = mkFalse
z3ValueSetConstraint (EmbeddedVar _ x) m (Embed vs) xVar = z3ValueSetConstraint (intVar x) m vs xVar

lookupLastAST :: SomeVar -> Map SomeVar [(AST,Int)] -> Maybe AST
lookupLastAST x m = fst . maximumOn snd <$> Map.lookup x m

lookupLastsAST :: NonEmpty SomeVar -> Map SomeVar [(AST,Int)] -> Maybe AST
lookupLastsAST xs m = fst . maximumOn snd . concat . NonEmpty.toList <$> traverse (`Map.lookup` m) xs
