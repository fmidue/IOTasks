{-# LANGUAGE TypeApplications #-}
module RegressionTests where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOrep
import Test.IOTest.IOProperty
import Test.IOTest.Translation
import Test.IOTest.Language
import Test.IOTest.Internal.TraceSet

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (within, sample)

regressionTests :: Spec
regressionTests = describe "Testing for regressions:" $ do
  prop "correctly break loop on end marker in interpretation" $
    let s = tillEnd $ end <> writeFixedOutput [buildPattern "X"]
        p = buildComputation @IOrep s
    in within 1000000 $ p `fulfills` s
  describe "fail with a runtime error for a toplevel loopEnd in traceGen and buildComputation" $ do
    specify "for buildComputation" $ buildComputation end `shouldThrow` anyErrorCall
    specify "for traceGen" $ sample (traceGen end) `shouldThrow` anyErrorCall
