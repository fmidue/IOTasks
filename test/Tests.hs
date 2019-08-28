{-# LANGUAGE TypeApplications #-}
import TestExamples
import RegressionTests

import Test.IOTest.IOProperty
import Test.IOTest.IOrep
import Test.IOTest.Examples.Hangman

import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec $ do
  testExamples
  describe "Testing hangman" $
    prop "game runs on a random 'words'" $ \xs -> let word = filter (`elem` hangmanDomain) xs in hangmanProg @IOrep word `fulfills` hangmanSpec word
  regressionTests
