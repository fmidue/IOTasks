{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module BoolVariablesSpec where

import Prelude hiding (readLn, print, getChar)

import Test.Hspec
import Test.IOTasks
import qualified Test.IOTasks.Random as Random

import Data.Functor ((<&>))

specification :: Specification
specification =
  readInput b bools AssumeValid <>
  readInput b bools AssumeValid <>
  branch (currentValue b)
    (writeOutput [ resultOf $ valueBefore 1 b ])
    (readInput b bools AssumeValid <>
    writeOutput [ resultOf $ currentValue b .||. valueBefore 2 b ]
    )
  where
    b = boolVar "b"

program :: MonadTeletype io => io ()
program = do
  x <- readLn
  y <- readLn
  if y
    then print x
    else do
      z <- readLn
      print (x || z)

spec :: Spec
spec = do
  context "boolean values" $ do
    describe "taskCheck program specification" $
      it "is success" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True

    describe "random testing" $ do
      it "works as expected" $
        (Random.taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True

    describe "static test-suite generation" $
      it "works for boolean values" $
        (((\is -> taskCheckOn is program specification) <$> generateStaticTestSuite stdArgs specification) <&> isSuccess) `shouldReturn` True
