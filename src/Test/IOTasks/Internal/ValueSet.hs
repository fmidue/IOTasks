{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Internal.ValueSet (
  ValueSet(..),
  empty, complete, singleton, fromList,
  lessThan, greaterThan,
  union, intersection,
  (\\), with, without,
  complement,
  isEmpty,
  containsValue,
  printValueSet,
  valueOf,
  Size(..),
  ints, nats, str,
  z3ValueSetConstraint,
  ) where

import Data.Set (Set)
import qualified Data.Set as Set (union,intersection,filter,toList,fromAscList, empty)
import Data.Typeable

import Test.QuickCheck

import Z3.Monad

data ValueSet a where
  Union :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
  Intersection :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
  GreaterThan :: Integer -> ValueSet Integer
  LessThan :: Integer -> ValueSet Integer
  Eq :: Integer -> ValueSet Integer
  Every :: ValueSet a
  None :: ValueSet a

data Size = Size { intAbs :: Integer, strLen :: Int }

empty :: ValueSet a
empty = None

complete :: ValueSet a
complete = Every

singleton :: Integer -> ValueSet Integer
singleton = Eq

fromList :: [Integer] -> ValueSet Integer
fromList = foldr (union . singleton) empty

greaterThan :: Integer -> ValueSet Integer
greaterThan = GreaterThan

lessThan :: Integer -> ValueSet Integer
lessThan = LessThan

union :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
union = Union

intersection :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
intersection = Intersection

(\\) :: ValueSet Integer -> ValueSet Integer -> ValueSet Integer
(\\) xs = (xs `intersection`) . complement

without :: ValueSet Integer -> Integer -> ValueSet Integer
without vs = (vs \\) . singleton

with :: ValueSet Integer -> Integer -> ValueSet Integer
with vs = (vs `union`) . singleton

complement :: ValueSet a -> ValueSet a
complement (GreaterThan n) = LessThan n `Union` Eq n
complement (LessThan n) =  GreaterThan n `Union` Eq n
complement (Intersection va vb) = Union (complement va) (complement vb)
complement (Union va vb) = Intersection (complement va) (complement vb)
complement (Eq n) = GreaterThan n `Union` LessThan n
complement Every = None
complement None = Every

containsValue :: ValueSet a -> a -> Bool
containsValue (Union vs1 vs2) n = vs1 `containsValue` n || vs2 `containsValue` n
containsValue (Intersection vs1 vs2) n = vs1 `containsValue` n && vs2 `containsValue` n
containsValue (GreaterThan i) n = n > i
containsValue (LessThan i) n = n < i
containsValue (Eq i) n = i == n
containsValue Every _ = True
containsValue None _ = False

valueOf :: forall a. Typeable a => ValueSet a -> Size -> Gen a
valueOf =
  case eqT @a @Integer of
    Just Refl -> valueOfInt
    Nothing -> case eqT @a @String of
      Just Refl -> valueOfString
      Nothing -> error $ "unsupported ValueSet type: " ++ show (typeRep $ Proxy @a)

valueOfInt :: ValueSet Integer -> Size -> Gen Integer
valueOfInt vs (Size sz _) =
  case Set.toList $ range vs $ Set.fromAscList [-sz..sz] of
    [] -> error $ unwords ["valueOf: no values between",show (-sz),"and",show sz,"within size bound for",printValueSet vs]
    xs -> elements xs
  where
    range :: ValueSet Integer -> Set Integer -> Set Integer
    range (Union x y) r = range x r `Set.union` range y r
    range (Intersection x y) r = range x r `Set.intersection` range y r
    range (GreaterThan n) r = Set.filter (>n) r
    range (LessThan n) r = Set.filter (<n) r
    range (Eq n) r = Set.filter (==n) r
    range Every r = r
    range None _ = Set.empty

valueOfString :: ValueSet String -> Size -> Gen String
valueOfString Every (Size _ len) = resize len . listOf $ elements ['a'..'z']
valueOfString None _ = error "valueOf: empty ValueSet"

printValueSet :: forall a. Typeable a => ValueSet a -> String
printValueSet vs = concat ["{ v : ",show (typeRep $ Proxy @a), " | ", printValueSet' vs ,"}"] where
  printValueSet' (Union vs1 vs2) = concat ["(",printValueSet' vs1,") \\/ (", printValueSet' vs2,")"]
  printValueSet' (Intersection vs1 vs2) = concat ["(",printValueSet' vs1,") /\\ (", printValueSet' vs2,")"]
  printValueSet' (GreaterThan n) = "v > " ++ show n
  printValueSet' (LessThan n) = "v < " ++ show n
  printValueSet' (Eq n) = "v == " ++ show n
  printValueSet' Every = "true"
  printValueSet' None = "false"

-- basic ValueSets
ints, nats :: ValueSet Integer
ints = Every
nats = Eq 0 `Union` GreaterThan 0

str :: ValueSet String
str = Every

--
isEmpty :: ValueSet Integer -> IO Bool
isEmpty vs = evalZ3 $ do
  x <- mkIntVar =<< mkStringSymbol "x"
  assert =<< z3ValueSetConstraint vs x
  res <- check
  pure $ case res of
    Sat -> False
    Unsat -> True
    Undef -> error "isEmpty: could not solve SMT constraints"

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
z3ValueSetConstraint (LessThan n) xVar = mkIntNum n >>= mkLt xVar
z3ValueSetConstraint (Eq n) xVar = mkIntNum n >>= mkEq xVar
z3ValueSetConstraint Every _ = mkTrue
z3ValueSetConstraint None _ = mkFalse
