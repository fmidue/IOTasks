module Testing where

import IOtt (IOtt, runProgram)
import Type
import Trace
import TraceSet

import Control.Arrow

import Test.QuickCheck

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

test :: IOtt () -> Spec VarName -> IO ()
test = quickCheck ... flip specProperty

specProperty :: Spec VarName -> IOtt () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAll gen prop

testTrace :: (Trace, [Int]) -> IOtt () -> Bool
testTrace (t,i) p = runProgram (show <$> i) p `lessGeneralThan` (show <$> t)
