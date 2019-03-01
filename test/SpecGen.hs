{-# LANGUAGE TypeApplications #-}
module SpecGen where

import Test.IOTest.Language

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

newtype VarSet = VarSet [VarName]

emptyVarSet :: VarSet
emptyVarSet = VarSet []

input :: VarSet -> Gen (Specification, VarSet)
input (VarSet vs) = do
  x <- elements ["x","y","z"]
  return (ReadInput x IntTy, VarSet $ x:vs)

output :: VarSet -> Gen (Specification, VarSet)
output (VarSet vs) = oneof [unary, binary] where
  unary = do
    f <- elements [sum, length]
    xs <- elements vs
    return (WriteOutput [UListF f xs], VarSet vs)
  binary = do
    f <- elements [(+), (-), (*)]
    x <- elements vs
    y <- elements vs
    return (WriteOutput [BIntF f (x,y)], VarSet vs)
