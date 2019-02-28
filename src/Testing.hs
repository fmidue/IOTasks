module Testing where

import IOtt (IOtt, runProgram)
import Language
import Trace
import TraceSet

import Control.Arrow ((&&&))
import Control.Monad.Trans.Writer.Lazy
import Test.QuickCheck
import Data.Bifunctor (bimap)

import qualified Data.Set as S

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

test :: IOtt () -> Specification -> IO ()
test = quickCheckWith stdArgs{chatty = False} ... flip specProperty

specProperty :: Specification -> IOtt () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAll gen prop

testTrace :: (NTrace Int, [Int]) -> IOtt () -> Property
testTrace (tg,i) p =
  let t = normalize $ runProgram (show <$> i) p
      w = t `isCoveredBy` bimap (S.map (fmap show)) show tg
      (result,msg) = runWriter w
  in counterexample (msg ++ "\n  program trace: " ++ show (bimap (S.map (fmap Wrap)) Wrap t)) result

newtype StringWrapper = Wrap String deriving (Eq,Ord)
instance Show StringWrapper where
  show (Wrap s) = s
