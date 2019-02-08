import Prelude hiding (getLine, putStrLn, print)
import Type
import Examples.Tasks
--import Examples.Hangman
import Testing

import Test.QuickCheck (expectFailure)
import Test.Hspec (hspec,it)

main :: IO ()
main = hspec $ do
  it "Testing solution1 against task1" $
    specProperty task1 solution1
  it "Testing wrongSolution1 against task1" $
    expectFailure $ specProperty task1 wrongSolution1
  --it "Testing solution2 against task2" $
  --  specProperty task2 solution2
  it "Testing solution3 against task3" $
    specProperty task3 solution3
  it"Testing solution1 against (task3 ∨ task1)" $
    specProperty (Choice task3 task1 Nop) solution1
  --it "Testing solution2 against (task1 ∨ task3)" $
  --  expectFailure $ specProperty (Choice [task1,task3]) solution2
  it "Testing solution3 against (task1 ∨ task3)" $
    specProperty (Choice task1 task3 Nop) solution3
  --it "Testing solution4 against task4" $
  --  specProperty task4 solution4
  --it "Testing wrongsolution4 against task4" $
  --  expectFailure $ specProperty task4 wrongSolution4
  --it "Testing hangman" $
  --  specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
