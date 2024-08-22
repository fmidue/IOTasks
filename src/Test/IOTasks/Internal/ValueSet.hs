{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Test.IOTasks.Internal.ValueSet (
  ValueSet(..),
  empty, complete, singleton, fromList,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  complement,
  unique, notInVar,
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
import Test.IOTasks.ValueMap (ValueMap, lookupInteger, emptyValueMap, lookupBool, lookupString)
import Test.IOTasks.Var (Var (..), SomeVar (..), someVar, intVar, Embedded (Embedded), Embeddable (..), Varname, varname)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)

data ValueSet a where
  Union :: ValueSet a -> ValueSet a -> ValueSet a
  Intersection :: ValueSet a -> ValueSet a -> ValueSet a
  GreaterThan :: Integer -> ValueSet Integer
  LessThan :: Integer -> ValueSet Integer
  Eq :: Integer -> ValueSet Integer
  CurrentlyIn :: VarRef a -> ValueSet a
  CurrentlyNotIn :: VarRef a -> ValueSet a

  Every :: ValueSet a
  None :: ValueSet a
  Embed :: Embeddable a => ValueSet Integer -> ValueSet (Embedded a)

deriving instance Eq (ValueSet a)
deriving instance Ord (ValueSet a)

data VarRef a = Self | Other (Var a)
  deriving (Eq,Ord)

hasVarRefs :: ValueSet a -> Bool
hasVarRefs Every = False
hasVarRefs None = False
hasVarRefs GreaterThan{} = False
hasVarRefs LessThan{} = False
hasVarRefs Eq{} = False
hasVarRefs (Embed vs) = hasVarRefs vs
hasVarRefs CurrentlyIn{} = True
hasVarRefs CurrentlyNotIn{} = True
hasVarRefs (Union x y) = hasVarRefs x || hasVarRefs y
hasVarRefs (Intersection x y) = hasVarRefs x || hasVarRefs y

data Size = Size { intAbs :: Integer, strLen :: Int }

empty :: ValueSet a
empty = None

complete :: ValueSet a
complete = Every

singleton :: Integer -> ValueSet Integer
singleton = Eq

fromList :: [Integer] -> ValueSet Integer
fromList = foldr (union . singleton) empty

unique :: ValueSet a -> ValueSet a
unique vs = CurrentlyNotIn Self `intersection` vs

notInVar :: ValueSet a -> Var a -> ValueSet a
notInVar vs y = CurrentlyNotIn (Other y) `intersection` vs

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
(\\) :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
(\\) xs = (xs `intersection`) . complement

without :: ValueSet Integer -> Integer -> ValueSet Integer
without vs = (vs \\) . singleton

with :: ValueSet Integer -> Integer -> ValueSet Integer
with vs = (vs `union`) . singleton

embed ::  Embeddable a => ValueSet Integer -> ValueSet (Embedded a)
embed vs
  | hasVarRefs vs = error "embed: cannot embed value sets with references to variables"
  | otherwise = Embed vs

embedFromList :: Embeddable a => [a] -> ValueSet (Embedded a)
embedFromList = embed . fromList . map asInteger

complement :: ValueSet a -> ValueSet a
complement (GreaterThan n) = LessThan n `union` Eq n
complement (LessThan n) =  GreaterThan n `union` Eq n
complement (Intersection va vb) = Union (complement va) (complement vb)
complement (Union va vb) = Intersection (complement va) (complement vb)
complement (Eq n) = GreaterThan n `union` LessThan n
complement (CurrentlyIn x) = CurrentlyNotIn x
complement (CurrentlyNotIn x) = CurrentlyIn x
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
containsValue _ _ (Eq i) n = i == n
containsValue x m (CurrentlyIn Self) n = withVarEq x $ n `elem` valuesIn x m
containsValue _ m (CurrentlyIn (Other y)) n = withVarEq y $ n `elem` valuesIn y m
containsValue x m (CurrentlyNotIn Self) n = withVarEq x $ n `notElem` valuesIn x m
containsValue _ m (CurrentlyNotIn (Other y)) n = withVarEq y $ n `notElem` valuesIn y m
containsValue _ _ Every _ = True
containsValue _ _ None _ = False
containsValue _ _ (Embed vs) v = containsEmbeddedValue vs v

withVarEq :: Var a -> (Eq a => r) -> r
withVarEq IntVar{} x = x
withVarEq BoolVar{} x = x
withVarEq StringVar{} x = x
withVarEq EmbeddedVar{} x = x

containsEmbeddedValue :: ValueSet Integer -> Embedded a -> Bool
containsEmbeddedValue Every _ = True
containsEmbeddedValue None _ = False
containsEmbeddedValue (Union a b) v = containsEmbeddedValue a v || containsEmbeddedValue b v
containsEmbeddedValue (Intersection a b) v = containsEmbeddedValue a v && containsEmbeddedValue b v
containsEmbeddedValue (GreaterThan i) (Embedded n) = i > n
containsEmbeddedValue (LessThan i) (Embedded n) = i < n
containsEmbeddedValue (Eq i) (Embedded n) = i == n
containsEmbeddedValue CurrentlyIn{} _ = error "not allowed"
containsEmbeddedValue CurrentlyNotIn{} _ = error "not allowed"

valuesIn :: Var a -> ValueMap -> [a]
valuesIn x@IntVar{} env =
  case lookupInteger (someVar x) env of
    Just values -> map fst values
    Nothing -> error $ unwords ["cannot find values for variable", show x , "in", show env, "(perhaps you misspelled a variable name)"]
valuesIn x@(EmbeddedVar (ty :: TypeRep x) _) env =
  case lookupInteger (withTypeable ty $ someVar x) env of
    Just values -> map (Embedded . fst) values
    Nothing -> error $ unwords ["cannot find values for variable", show x , "in", show env, "(perhaps you misspelled a variable name)"]
valuesIn x@BoolVar{} env =
  case lookupBool (someVar x) env of
    Just values -> map fst values
    Nothing -> error $ unwords ["cannot find values for variable", show x , "in", show env, "(perhaps you misspelled a variable name)"]
valuesIn x@StringVar{} env =
  case lookupString (someVar x) env of
    Just values -> map fst values
    Nothing -> error $ unwords ["cannot find values for variable", show x , "in", show env, "(perhaps you misspelled a variable name)"]


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
    range (Eq n) r = Set.filter (==n) r
    range (CurrentlyIn Self) r = Set.filter (\v -> f v `elem` valuesIn (g x) m) r
    range (CurrentlyIn (Other y)) r = Set.filter (\v -> f v `elem` valuesIn (g $ varname y) m) r
    range (CurrentlyNotIn Self) r = Set.filter (\v -> f v `notElem` valuesIn (g x) m) r
    range (CurrentlyNotIn (Other y)) r = Set.filter (\v -> f v `notElem` valuesIn (g $ varname y) m) r
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
    range (CurrentlyIn Self) r = Set.filter (\v -> v `elem` valuesIn x m) r
    range (CurrentlyIn (Other y)) r = Set.filter (\v -> v `elem` valuesIn y m) r
    range (CurrentlyNotIn Self) r = Set.filter (\v -> v `notElem` valuesIn x m) r
    range (CurrentlyNotIn (Other y)) r = Set.filter (\v -> v `notElem` valuesIn y m) r
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
reduceStringSet x m (CurrentlyIn Self) = FromThese (valuesIn x m)
reduceStringSet _ m (CurrentlyIn (Other y)) = FromThese (valuesIn y m)
reduceStringSet x m (CurrentlyNotIn Self) = NotThese (valuesIn x m)
reduceStringSet _ m (CurrentlyNotIn (Other y)) = NotThese (valuesIn y m)
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
  showValueSet' (Eq n) = "v == " ++ show n
  showValueSet' (CurrentlyIn ref) = "currentlyIn " ++ showVarRef ref
  showValueSet' (CurrentlyNotIn ref) = "currentlyNotIn " ++ showVarRef ref
  showValueSet' Every = "true"
  showValueSet' None = "false"
  showValueSet' (Embed vs) = "Embed " ++ showValueSet' vs

  showVarRef Self = "self"
  showVarRef (Other y) = show y

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 `Union` GreaterThan 0

bools :: ValueSet Bool
bools = complete

str :: ValueSet String
str = Every

--
-- | Check if a given 'ValueSet' of integers is empty.
--
-- This function uses an external SMT solver to check the constraints defined by the 'ValueSet'.
isEmpty :: Var Integer -> Map SomeVar [AST] -> ValueSet Integer -> IO Bool
isEmpty var m vs = evalZ3 $ do
  x <- mkIntVar =<< mkStringSymbol "x"
  assert =<< z3ValueSetConstraint var m vs x
  res <- check
  pure $ case res of
    Sat -> False
    Unsat -> True
    Undef -> error "isEmpty: could not solve SMT constraints"

z3ValueSetConstraint :: MonadZ3 z3 => Typeable a => Var a -> Map SomeVar [AST] -> ValueSet a -> AST -> z3 AST
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
z3ValueSetConstraint _ _ (Eq n) xVar = mkIntNum n >>= mkEq xVar
z3ValueSetConstraint var m (CurrentlyIn Self) xVar = do
  let xs = fromMaybe [] $ Map.lookup (someVar var) m
  mkOr =<< sequence [ mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint _ m (CurrentlyIn (Other y)) xVar = do
  let xs = fromMaybe [] $ Map.lookup (someVar y) m
  mkOr =<< sequence [ mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint var m (CurrentlyNotIn Self) xVar = do
  let xs = fromMaybe [] $ Map.lookup (someVar var) m
  mkAnd =<< sequence [ mkNot =<< mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint _ m (CurrentlyNotIn (Other y)) xVar = do
  let xs = fromMaybe [] $ Map.lookup (someVar y) m
  mkAnd =<< sequence [ mkNot =<< mkEq xVar x | x <- xs, x /= xVar]
z3ValueSetConstraint x@(EmbeddedVar (_ :: TypeRep x) _) m Every xVar = z3ValueSetConstraint x m (Embed $ fromList (embeddingRange @x)) xVar
z3ValueSetConstraint _ _ Every _ = mkTrue
z3ValueSetConstraint _ _ None _ = mkFalse
z3ValueSetConstraint (EmbeddedVar _ x) m (Embed vs) xVar = z3ValueSetConstraint (intVar x) m vs xVar
