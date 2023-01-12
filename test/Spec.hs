import Test.Hspec

import TestExamples
import TestProperties

main :: IO ()
main = hspec $ do
  testExamples
  testProperties
