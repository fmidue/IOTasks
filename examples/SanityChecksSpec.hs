module SanityChecksSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Control.Exception (ErrorCall (ErrorCall))
import Data.List (isInfixOf)

wrong1 :: Specification
wrong1 = while undefined (branch undefined nop exit)

wrong2 :: Specification
wrong2 = whileNot undefined (branch undefined exit nop)

wrong3 :: Specification
wrong3 = repeatUntil (branch undefined nop exit) undefined

wrong4 :: Specification
wrong4 = doWhile (branch undefined exit nop) undefined

wrong5 :: Specification
wrong5 = tillExit nop

spec :: Spec
spec =
  context "sanity checking for specifications" $ do
    describe "wrong1" $ do
      it "throws error" $
          taskCheck undefined wrong1 `shouldThrow` errorContaining "while: top-level exit marker in body"
    describe "wrong2" $ do
      it "throws error" $
          taskCheck undefined wrong2 `shouldThrow` errorContaining "whileNot: top-level exit marker in body"
    describe "wrong3" $ do
      it "throws error" $
          taskCheck undefined wrong3 `shouldThrow` errorContaining "repeatUntil: top-level exit marker in body"
    describe "wrong4" $ do
      it "throws error" $
          taskCheck undefined wrong4 `shouldThrow` errorContaining "doWhile: top-level exit marker in body"
    describe "wrong5" $ do
      it "throws error" $
          taskCheck undefined wrong5 `shouldThrow` errorContaining "tillExit: no top-level exit marker in body"

errorContaining :: String -> Selector ErrorCall
errorContaining str (ErrorCall msg) = str `isInfixOf` msg
