{-# LANGUAGE TypeApplications #-}
module Testing where

import IOtt
import Type
import Matching

import Data.Either

import Test.QuickCheck

test :: IOtt () -> Spec VarName -> IO ()
test program spec = quickCheck $ specProperty spec program

specProperty :: Spec VarName -> IOtt () -> Property
specProperty spec program =
  let gen = inputGenerator spec
      prop inputs = let trace = runtt program (inputs ++ ["0"])
                    in testTrace (changeCarrier read trace) spec
  in forAll gen prop

testTrace :: Trace' Int () -> Spec VarName -> Property
testTrace trace spec =
  let res = trace `matches` spec
      msg = fromLeft "" res
  in counterexample msg $ isRight res

inputGenerator :: Spec a -> Gen [String]
inputGenerator _s = vectorOf 11 $ show <$> choose @Int (1,10)
