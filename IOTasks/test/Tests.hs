{-# LANGUAGE TypeApplications #-}
import TestExamples
import TestPTermExamples
import RegressionTests

import Test.IOTasks (fulfills)
import Test.IOTasks.IOrep
import Test.IOTasks.Examples.Hangman

import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck (prop)

import Debug.Trace

main :: IO ()
main = hspec $ do
  testExamples
  testPTermExamples
  describe "Testing hangman" $
    prop "game runs on a random 'words'" $
      \xs -> let word = filter (`elem` hangmanDomain) xs
             in hangmanProg @IOrep word `fulfills` hangmanSpec word
  regressionTests
