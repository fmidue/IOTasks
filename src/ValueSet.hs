module ValueSet where

import Data.List (intersect, union)

import Test.QuickCheck

data ValueSet
  = Union ValueSet ValueSet
  | Intersection ValueSet ValueSet
  | Complement ValueSet
  | GreaterThan Int
  | LessThen Int
  | Eq Int
  | Every

type Size = Int

valueOf :: ValueSet -> Size -> Gen Int
valueOf vs sz = elements $ range vs [-sz..sz] where
  range :: ValueSet -> [Int] -> [Int]
  range (Union x y) r = range x r `union` range y r
  range (Intersection x y) r = range x r `intersect` range y r
  range (Complement x) r = filter (`notElem` range x r) [-sz .. sz]
  range (GreaterThan n) r = filter (>n) r
  range (LessThen n) r = filter (<n) r
  range (Eq n) r = filter (==n) r
  range Every r = r
