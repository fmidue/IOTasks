import TestExamples

import Test.Hspec (hspec)

main :: IO ()
main = hspec testExamples

    --it "Testing hangman" $
    --  specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
