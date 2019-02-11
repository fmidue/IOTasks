import Prelude hiding (getLine, putStrLn, print)
import Type
import Examples.Tasks
--import Examples.Hangman
import Testing

import Test.Hspec (hspec,it)
import Test.Hspec.Expectations

main :: IO ()
main = hspec $ do
  it "solution1 matches task1" $
    test solution1 task1 `shouldReturn` True
  it "wrongSolution1 does not match against task1" $
    test wrongSolution1 task1 `shouldReturn` False
  --it "Testing solution2 against task2" $
  --  specProperty task2 solution2
  it "solution3 matches task3" $
    test solution3 task3 `shouldReturn` True
  it"solution1 matches (task3 ∨ task1)" $
    test solution1 (Choice task3 task1 Nop) `shouldReturn` True
  --it "Testing solution2 against (task1 ∨ task3)" $
  --  expectFailure $ specProperty (Choice [task1,task3]) solution2
  it "solution3 matches (task1 ∨ task3)" $
    test solution3 (Choice task1 task3 Nop) `shouldReturn` True
  --it "Testing solution4 against task4" $
  --  specProperty task4 solution4
  --it "Testing wrongsolution4 against task4" $
  --  expectFailure $ specProperty task4 wrongSolution4
  --it "Testing hangman" $
  --  specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
