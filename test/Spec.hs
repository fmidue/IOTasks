import Prelude hiding (getLine, putStrLn, print)
import Types
import Examples.Tasks
import Examples.Hangman
import Testing

import Test.QuickCheck (expectFailure)
import Test.Hspec (hspec,it)

main :: IO ()
main = hspec $ do
  it "Testing solution1 against task1" $
    specProperty task1 solution1
  it "Testing solution2 against task2" $
    specProperty task2 solution2
  it "Testing solution3 against task3" $
    specProperty task3 solution3
  it"Testing solution1 against (task1 ∨ task3)" $
    specProperty (Choice [task1,task3]) solution1
  it "Testing solution2 against (task1 ∨ task3)" $
    expectFailure (specProperty (Choice [task1,task3]) solution2)
  it "Testing solution3 against (task1 ∨ task3)" $
    specProperty (Choice [task1,task3]) solution3
  it "Testing hangman" $
    specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
