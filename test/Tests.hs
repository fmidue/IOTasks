import TestSimple
import TestExtended

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  testSimple
  testExtended

    --it "Testing hangman" $
    --  specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
