import Prelude hiding (getLine, putStrLn, print)
import Examples.Tasks
--import Examples.Hangman
import Testing
import SpecGen
import Solution

import Test.Hspec (hspec,it)
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  it "solution1 matches task1" $
    specProperty task1 solution1
  it "wrongSolution1 does not match against task1" $
    expectFailure $ specProperty task1 wrongSolution1
  it "solution1 matches task1'" $
    specProperty task1' solution1
  it "solution1' matches task1'" $
    specProperty task1' solution1'
  it "solution2 matches task2" $
   specProperty task2 solution2
  it "solution3 matches task3" $
    specProperty task3 solution3
  it "solution3 matches task3'" $
    specProperty task3' solution3
  -- it"solution1 matches (task3 ∨ task1)" $
  --   test solution1 (Choice task3 task1 Nop) `shouldReturn` True
  --it "Testing solution2 against (task1 ∨ task3)" $
  --  expectFailure $ specProperty (Choice [task1,task3]) solution2
  -- it "solution3 matches (task1 ∨ task3)" $
  --   test solution3 (Choice task1 task3 Nop) `shouldReturn` True
  --it "Testing solution4 against task4" $
  --  specProperty task4 solution4
  --it "Testing wrongsolution4 against task4" $
  --  expectFailure $ specProperty task4 wrongSolution4
  --it "Testing hangman" $
  --  specProperty (hangmanSpec [2,7,1,4,2,1]) (hangmanProg [2,7,1,4,2,1])
  it "programs build from a simple spec (read and write only) satisfy that spec" $
    forAll specGen (\s -> specProperty s (buildProgram s))
