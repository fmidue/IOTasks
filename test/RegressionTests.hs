{-# LANGUAGE TypeApplications #-}
module RegressionTests where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest
import Test.IOTest.Term.ATerm
import Test.IOTest.TraceSet

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (within, sample)

regressionTests :: Spec
regressionTests = describe "Testing for regressions:" $ do
  prop "correctly break loop on exit marker in interpretation" $
    let s = tillExit $ exit <> writeFixedOutput [buildTermPattern "X"]
        p = buildComputation @ATerm @IOrep s
    in within 1000000 $ fulfills @_ @(Specification ATerm) p s

  describe "fail with a runtime error for a toplevel 'throwError Exit' in traceGen and buildComputation" $ do
    specify "for buildComputation" $ buildComputation @ATerm exit `shouldThrow` anyErrorCall
    specify "for traceGen" $ sample (traceGen @ATerm exit) `shouldThrow` anyErrorCall
