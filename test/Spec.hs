import Test.Hspec

import TestExamples
import TestProperties

import Control.Monad (when)
import System.Environment (getArgs, withArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--cheap":xs -> withArgs xs testCheap
    "--all":xs -> withArgs xs testAll
    _ -> testAll


testCheap, testAll :: IO ()
testCheap = test False
testAll = test True

test :: Bool -> IO ()
test testExpensive =
  hspec $ do
    testExamples
    testCheapProperties
    when testExpensive testExpensiveProperties
