{-# LANGUAGE TypeApplications #-}
module PalindromeSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

palindromeSpec :: Specification
palindromeSpec =
  readInput x str AssumeValid <>
  branch (currentValue x .==. reverse' (currentValue x))
    (writeOutput [text "Yes"])
    (writeOutput [text "No"])
  where
    x = stringVar "x"

palindromeProg :: MonadTeletype m => m ()
palindromeProg = do
  str <- getLine
  if str == reverse str
    then putStrLn "Yes"
    else putStrLn "No"

spec :: Spec
spec =
  context "string inputs" $ do
    describe "palindromeProg" $ do
      it "satisfies palindromeProg" $
        (taskCheckOutcome palindromeProg palindromeSpec <&> isSuccess) `shouldReturn` True
