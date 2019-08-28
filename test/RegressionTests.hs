{-# LANGUAGE TypeApplications #-}
module RegressionTests where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOrep
import Test.IOTest.IOProperty
import Test.IOTest.Translation
import Test.IOTest.Language

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (within)

regressionTests :: Spec
regressionTests = describe "Testing for regressions:" $
  prop "correctly break loop on end marker in interpretation" $
    let s = tillEnd $ end <> writeFixedOutput [buildPattern "X"]
        p = buildProgram @IOrep s
    in within 1000000 $ p `fulfills` s
