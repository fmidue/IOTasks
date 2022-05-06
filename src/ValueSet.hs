module ValueSet where

import Data.List (intersect, union)

import Test.QuickCheck

data ValueSet
  = Union ValueSet ValueSet
  | Intersection ValueSet ValueSet
  | GreaterThan Integer
  | LessThen Integer
  | Eq Integer
  | Every
  | None

type Size = Integer

complement :: ValueSet -> ValueSet
complement (GreaterThan n) = LessThen n `Union` Eq n
complement (LessThen n) =  GreaterThan n `Union` Eq n
complement (Intersection va vb) = Union (complement va) (complement vb)
complement (Union va vb) = Intersection (complement va) (complement vb)
complement (Eq n) = GreaterThan n `Union` LessThen n
complement Every = None
complement None = Every

containsValue :: ValueSet -> Integer -> Bool
containsValue (Union vs1 vs2) n = vs1 `containsValue` n || vs2 `containsValue` n
containsValue (Intersection vs1 vs2) n = vs1 `containsValue` n && vs2 `containsValue` n
containsValue (GreaterThan i) n = n > i
containsValue (LessThen i) n = n < i
containsValue (Eq i) n = i == n
containsValue Every _ = True
containsValue None _ = False

valueOf :: ValueSet -> Size -> Gen Integer
valueOf vs sz =
  case range vs [-sz..sz] of
    [] -> error "valueOf: no values within size bound"
    xs -> elements xs
  where
    range :: ValueSet -> [Integer] -> [Integer]
    range (Union x y) r = range x r `union` range y r
    range (Intersection x y) r = range x r `intersect` range y r
    range (GreaterThan n) r = filter (>n) r
    range (LessThen n) r = filter (<n) r
    range (Eq n) r = filter (==n) r
    range Every r = r
    range None _ = error "valueOf: empty ValueSet"

printValueSet :: ValueSet -> String
printValueSet vs = concat ["{ v : Int | ", printValueSet' vs ,"}"] where
  printValueSet' (Union vs1 vs2) = concat ["(",printValueSet' vs1,") \\/ (", printValueSet' vs2,")"]
  printValueSet' (Intersection vs1 vs2) = concat ["(",printValueSet' vs1,") /\\ (", printValueSet' vs2,")"]
  printValueSet' (GreaterThan n) = "v > " ++ show n
  printValueSet' (LessThen n) = "v < " ++ show n
  printValueSet' (Eq n) = "v == " ++ show n
  printValueSet' Every = "true"
  printValueSet' None = "false"
