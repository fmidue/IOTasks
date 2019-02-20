{-# LANGUAGE TypeApplications #-}
module SpecGen where

import Language

import Test.QuickCheck hiding (Positive,output)

specGen :: Gen Specification
specGen = do
  (s,vs) <- input emptyVarSet
  n <- choose @Int (0, 10)
  ss <- go n [s] vs
  return $ foldr1 (<>) (reverse ss)

go :: Int -> [Specification] -> VarSet -> Gen [Specification]
go 0 ss _ = return ss
go n ss vs = do
  (s, vs') <- oneof [input vs, output vs]
  go (n-1) (s:ss) vs'

data VarSet = VarSet { locals :: [VarName], globals :: [VarName]}

emptyVarSet :: VarSet
emptyVarSet = VarSet [] []

input :: VarSet -> Gen (Specification, VarSet)
input vs = do
  x <- elements ["x","y","z"]
  xs <- elements ["xs","ys","zs"]
  return (ReadInput x IntTy xs, vs{locals = x:locals vs, globals=xs:globals vs})

output :: VarSet -> Gen (Specification, VarSet)
output vs = oneof [unary, binary] where
  unary = do
    f <- elements [sum, length]
    xs <- elements $ globals vs
    return (WriteOutput [UListF f xs],vs)
  binary = do
    f <- elements [(+), (-), (*)]
    x <- elements $ locals vs
    y <- elements $ locals vs
    return (WriteOutput [BIntF f (x,y)],vs)
