module Testing (test) where

import IOtt
import Type
import Matching

import Data.Bifunctor
import Data.Either

import Test.QuickCheck

test :: IOtt () -> Spec VarName -> IO ()
test program spec = quickCheck $ specProperty program spec

specProperty :: IOtt () -> Spec VarName -> Property
specProperty program spec =
  let gen = vectorOf 11 $ show <$> intGen
      prop inputs = testTrace (first read (runtt program (inputs ++ ["0"]))) spec
  in forAll gen prop

testTrace :: Trace' Int () -> Spec VarName -> Property
testTrace trace spec =
  let res = trace `matches` spec
      msg = fromLeft "" res
  in counterexample msg $ isRight res

intGen :: Gen Int
intGen = choose (1,10)
