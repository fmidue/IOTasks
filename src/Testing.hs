module Testing where

import IOtt (IOtt, runProgram)
import Language
import Trace
import TraceSet

import Control.Arrow
import           Control.Monad.Trans.Writer.Lazy

import Test.QuickCheck

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

test :: IOtt () -> Specification -> IO ()
test = quickCheck ... flip specProperty

specProperty :: Specification -> IOtt () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAll gen prop

testTrace :: (GTrace Int, [Int]) -> IOtt () -> Property
testTrace (t,i) p =
  let w = (show <$> t) `covers` runProgram (show <$> i) p
      (result,msg) = runWriter w
  in counterexample msg result
