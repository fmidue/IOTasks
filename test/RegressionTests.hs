{-# LANGUAGE TypeApplications #-}
module RegressionTests where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.IOrep
import Test.IOTest.IOProperty
import Test.IOTest.Translation
import Test.IOTest.Language
import Test.IOTest.TraceSet

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (within, sample)

regressionTests :: Spec
regressionTests = describe "Testing for regressions:" $ do
  prop "correctly break loop on exit marker in interpretation" $
    let s = tillExit $ exit <> writeFixedOutput [buildPattern "X"]
        p = buildComputation @IOrep s
    in within 1000000 $ p `fulfills` s
  describe "fail with a runtime error for a toplevel 'throwError Exit' in traceGen and buildComputation" $ do
    specify "for buildComputation" $ buildComputation exit `shouldThrow` anyErrorCall
    specify "for traceGen" $ sample (traceGen exit) `shouldThrow` anyErrorCall
