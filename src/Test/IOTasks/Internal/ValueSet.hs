{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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
import qualified Data.Set as Set (union,intersection,filter,toList,fromAscList, empty)
import Type.Reflection

import Test.QuickCheck

import Z3.Monad
import Test.IOTasks.ValueMap (ValueMap, lookupInteger, emptyValueMap)
import Test.IOTasks.Var (Var (..), SomeVar (..), someVar, intVar, Embedded (Embedded), Embeddable (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data ValueSet a where
  Union :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
  Intersection :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
  GreaterThan :: Integer -> ValueSet Integer
  LessThan :: Integer -> ValueSet Integer
  Eq :: Integer -> ValueSet Integer
  CurrentlyIn :: VarRef -> ValueSet Integer
  CurrentlyNotIn :: VarRef -> ValueSet Integer
  Every :: ValueSet a
  None :: ValueSet a
  Embed :: Embeddable a => ValueSet Integer -> ValueSet (Embedded a)

deriving instance Eq (ValueSet a)
deriving instance Ord (ValueSet a)

data VarRef = Self | Other (Var Integer)
  deriving (Eq,Ord)

data Size = Size { intAbs :: Integer, strLen :: Int }

empty :: ValueSet a
empty = None

complete :: ValueSet a
complete = Every

singleton :: Integer -> ValueSet Integer
singleton = Eq

fromList :: [Integer] -> ValueSet Integer
fromList = foldr (union . singleton) empty

unique :: ValueSet Integer -> ValueSet Integer
unique vs = CurrentlyNotIn Self `intersection` vs

notInVar :: ValueSet Integer -> Var Integer -> ValueSet Integer
notInVar vs y = CurrentlyNotIn (Other y) `intersection` vs

greaterThan :: Integer -> ValueSet Integer
greaterThan = GreaterThan

lessThan :: Integer -> ValueSet Integer
lessThan = LessThan

union :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
union = Union

intersection :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
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
embed = Embed

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

embedComplement :: forall a. Embeddable a => ValueSet (Embedded a) -> ValueSet (Embedded a)
embedComplement Every = None
embedComplement None = Every
embedComplement (Embed vs) = Embed (fromList (embeddingRange @a) \\ vs)

containsValue :: Var a -> ValueMap -> ValueSet a -> a -> Bool
containsValue x m (Union vs1 vs2) n = containsValue x m vs1  n || containsValue x m vs2 n
containsValue x m (Intersection vs1 vs2) n = containsValue x m vs1 n && containsValue x m vs2 n
containsValue _ _ (GreaterThan i) n = n > i
containsValue _ _ (LessThan i) n = n < i
containsValue _ _ (Eq i) n = i == n
containsValue x m (CurrentlyIn Self) n = n `elem` valuesIn x m
containsValue _ m (CurrentlyIn (Other y)) n = n `elem` valuesIn y m
containsValue x m (CurrentlyNotIn Self) n = n `notElem` valuesIn x m
containsValue _ m (CurrentlyNotIn (Other y)) n = n `notElem` valuesIn y m
containsValue _ _ Every _ = True
containsValue _ _ None _ = False
containsValue (EmbeddedVar _ x) m (Embed vs) (Embedded i) = containsValue (intVar x) m vs i

valuesIn :: Var Integer -> ValueMap -> [Integer]
valuesIn x env =
  case lookupInteger (someVar x) env of
    Just values -> map fst values
    Nothing -> error $ unwords ["cannot find values for variable", show x , "in", show env, "(perhaps you misspelled a variable name)"]

initiallyContainsValue ::  ValueSet Integer -> Integer -> Bool
initiallyContainsValue = containsValue (intVar "x") $ emptyValueMap [someVar $ intVar "x"]

valueOf :: Var a -> ValueMap -> ValueSet a -> Size -> Gen a
valueOf _ _ None = error "valueOf: cannot draw value from empty value set"
valueOf x@IntVar{} m vs = valueOfInt x m vs
valueOf BoolVar{} _ Every = const $ elements [True,False]
valueOf StringVar{} _ vs = valueOfString vs
valueOf (EmbeddedVar _ x) m (Embed vs) = fmap Embedded . valueOfInt (intVar x) m vs
valueOf x@(EmbeddedVar (_ :: TypeRep x) _) m Every = valueOf x m (embed $ fromList (embeddingRange @x))

valueOfInt :: Var Integer -> ValueMap -> ValueSet Integer -> Size -> Gen Integer
valueOfInt x m vs (Size sz _) =
  case Set.toList $ range vs $ Set.fromAscList [-sz..sz] of
    [] -> error $ unwords ["valueOf: no values between",show (-sz),"and",show sz,"within size bound for",showValueSet vs]
    xs -> elements xs
  where
    range :: ValueSet Integer -> Set Integer -> Set Integer
    range (Union x y) r = range x r `Set.union` range y r
    range (Intersection x y) r = range x r `Set.intersection` range y r
    range (GreaterThan n) r = Set.filter (>n) r
    range (LessThan n) r = Set.filter (<n) r
    range (Eq n) r = Set.filter (==n) r
    range (CurrentlyIn Self) r = Set.filter (\v -> v `elem` valuesIn x m) r
    range (CurrentlyIn (Other y)) r = Set.filter (\v -> v `elem` valuesIn y m) r
    range (CurrentlyNotIn Self) r = Set.filter (\v -> v `notElem` valuesIn x m) r
    range (CurrentlyNotIn (Other y)) r = Set.filter (\v -> v `notElem` valuesIn y m) r
    range Every r = r
    range None _ = Set.empty

valueOfString :: ValueSet String -> Size -> Gen String
valueOfString Every (Size _ len) = resize len . listOf $ elements ['a'..'z']
valueOfString None _ = error "valueOf: empty ValueSet"

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

z3ValueSetConstraint :: MonadZ3 z3 => Var a -> Map SomeVar [AST] -> ValueSet a -> AST -> z3 AST
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
