module SanityChecksSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Control.Exception (ErrorCall (ErrorCall))
import Data.List (isInfixOf)

-- while
wrongWhile1 :: Specification
wrongWhile1 = while true (branch undefined nop exit)

wrongWhile2 :: Specification
wrongWhile2 = while true nop

wrongWhile3 :: Specification
wrongWhile3 = while (currentValue (intVar "x") .==. intLit 0) (readInput (intVar "y") undefined undefined)

-- whileNotNot
wrongWhileNot1 :: Specification
wrongWhileNot1 = whileNot true (branch undefined exit nop)

wrongWhileNot2 :: Specification
wrongWhileNot2 = whileNot true nop

wrongWhileNot3 :: Specification
wrongWhileNot3 = whileNot (currentValue (intVar "x") .==. intLit 0) (readInput (intVar "y") undefined undefined)

-- repeatUntil
wrongRepeat1 :: Specification
wrongRepeat1 = repeatUntil (branch undefined nop exit) false

wrongRepeat2 :: Specification
wrongRepeat2 = repeatUntil nop false

wrongRepeat3 :: Specification
wrongRepeat3 = repeatUntil (readInput (intVar "y") undefined undefined) (currentValue (intVar "x") .==. intLit 0)

-- doWhile
wrongDo1 :: Specification
wrongDo1 = doWhile (branch undefined exit nop) true

wrongDo2 :: Specification
wrongDo2 = doWhile nop false

wrongDo3 :: Specification
wrongDo3 = doWhile (readInput (intVar "y") undefined undefined) (currentValue (intVar "x") .==. intLit 0)

-- tillExit
wrongLoop :: Specification
wrongLoop = tillExit nop

spec :: Spec
spec =
  context "sanity checking for specifications" $ do
    describe "while" $ do
      it "throws error on top-level exit marker" $
        seq wrongWhile1 (pure ()) `shouldThrow` errorContaining "while: top-level exit marker in body"
      it "throws error on constant condition" $
        seq wrongWhile2 (pure ()) `shouldThrow` errorContaining "while: constant loop condition"
      it "throws error on missing progress" $
        seq wrongWhile3 (pure ()) `shouldThrow` errorContaining "while: body does not change the evaluation of the condition"

    describe "whileNot" $ do
      it "throws error on top-level exit marker" $
        seq wrongWhileNot1 (pure ()) `shouldThrow` errorContaining "whileNot: top-level exit marker in body"
      it "throws error on constant condition" $
        seq wrongWhileNot2 (pure ()) `shouldThrow` errorContaining "whileNot: constant loop condition"
      it "throws error on missing progress" $
        seq wrongWhileNot3 (pure ()) `shouldThrow` errorContaining "whileNot: body does not change the evaluation of the condition"

    describe "repeatUntil" $ do
      it "throws error on top-level exit marker" $
        seq wrongRepeat1 (pure ()) `shouldThrow` errorContaining "repeatUntil: top-level exit marker in body"
      it "throws error on constant condition" $
        seq wrongRepeat2 (pure ()) `shouldThrow` errorContaining "repeatUntil: constant loop condition"
      it "throws error on missing progress" $
        seq wrongRepeat3 (pure ()) `shouldThrow` errorContaining "repeatUntil: body does not change the evaluation of the condition"

    describe "doWhile" $ do
      it "throws error on top-level exit marker" $
        seq wrongDo1 (pure ()) `shouldThrow` errorContaining "doWhile: top-level exit marker in body"
      it "throws error on constant condition" $
        seq wrongDo2 (pure ()) `shouldThrow` errorContaining "doWhile: constant loop condition"
      it "throws error on missing progress" $
        seq wrongDo3 (pure ()) `shouldThrow` errorContaining "doWhile: body does not change the evaluation of the condition"

    describe "tillExit" $ do
      it "throws error because no exit marker" $
          taskCheck undefined wrongLoop `shouldThrow` errorContaining "tillExit: no top-level exit marker in body"

errorContaining :: String -> Selector ErrorCall
errorContaining str (ErrorCall msg) = str `isInfixOf` msg
