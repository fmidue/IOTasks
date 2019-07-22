{-# LANGUAGE TypeApplications #-}
import TestExamples

import Test.IOTest.IOProperty
import Test.IOTest.IOtt
import Test.IOTest.Examples.Hangman

import Test.Hspec (hspec, describe)
import Test.Hspec.QuickCheck (prop)

main :: IO ()
main = hspec $ do
  testExamples
  describe "Testing hangman" $
    prop "game runs on a random 'words'" $ \xs -> let word = filter (`elem` hangmanDomain) xs in hangmanProg @IOtt word `fulfills` hangmanSpec word
