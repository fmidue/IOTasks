{-# LANGUAGE TypeApplications #-}
module VariableMerging2Spec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

-- variable merging
specification :: Specification
specification =
  readInput x ints AssumeValid <>
  readInput y ints AssumeValid <>
  branch (currentValue y .>. intLit 0)
    (readInput x ints AssumeValid <>
    readInput x ints AssumeValid)
    (readInput y ints AssumeValid)
    <>
  branch (currentValue [x,y] .>. intLit 5)
    (readInput z ints AssumeValid)
    (readInput y ints AssumeValid)
    <>
  writeOutput [value $ as @Integer $ currentValue [x,z]] <>
  writeOutput [value $ length' $ as @[Integer] $ allValues [x,y,z]] <>
  writeOutput [value $ length' $ as @[Integer] $ allValues [x,y,a,z]]
  where
    x = intVar "x"
    y = intVar "y"
    z = intVar "z"
    a = intVar "a"

program :: MonadTeletype m => m ()
program = do
  x1 <- readLn @_ @Integer
  y1 <- readLn @_ @Integer
  st <- cond1 x1 y1
  (xs,ys,zs,c) <- cond2 st
  print c
  print $ length $ xs ++ ys ++ zs
  print $ length $ xs ++ ys ++ zs
  where
    cond1 x1 y1 =
      if y1 > 0
        then do
          x2 <- readLn @_ @Integer
          x3 <- readLn @_ @Integer
          pure ([x3,x2,x1],[y1],x3)
        else do
          y2 <- readLn @_ @Integer
          pure ([x1],[y2,y1],y2)
    cond2 (xs,ys,c) =
      if c > 5
        then do
          z <- readLn @_ @Integer
          pure (xs,ys,[z],z)
        else do
          y3 <- readLn @_ @Integer
          pure (xs,y3:ys,[],head xs)

spec :: Spec
spec =
  context "variable merging" $ do
    describe "taskCheck program specification" $
      it "is success" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True
    describe "interpretation of specification" $
      it "satisfies the specification" $
        (taskCheckOutcome (head $ interpret specification) specification <&> isSuccess) `shouldReturn` True
