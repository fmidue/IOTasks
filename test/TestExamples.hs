{-# LANGUAGE DataKinds #-}
module TestExamples (testExamples) where
import Test.Hspec

import Test.IOTasks

import qualified Test.IOTasks.Random.Testing as Random
import Example

import Control.Monad.Loops (allM)

import Data.Functor ((<&>))

testExamples :: Spec
testExamples = do
  context "solver based testing" $ do
    describe "prog1" $ do
      it "taskCheck example1 specification" $
        (taskCheckOutcome prog1 example1 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example2 specification" $
        (taskCheckOutcome prog1 example2 <&> isFailure) `shouldReturn` True
      it "does not fulfill example3 specification" $
        (taskCheckOutcome prog1 example3 <&> isFailure) `shouldReturn` True

    describe "prog2" $ do
      it "taskCheck example2 specification" $
        (taskCheckOutcome prog2 example2 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example1 specification" $
        (taskCheckOutcome prog2 example1 <&> isFailure) `shouldReturn` True
      it "does not fulfill example3 specification" $
        (taskCheckOutcome prog2 example3 <&> isFailure) `shouldReturn` True

    describe "prog2'" $ do
      it "taskCheck example2 specification" $
        (taskCheckOutcome prog2' example2 <&> isSuccess) `shouldReturn` False

    describe "prog2''" $ do
      it "taskCheck example2' specification with overflow warnings" $
        (taskCheckWithOutcome stdArgs{avoidOverflows=False} prog2'' example2' <&> (\o -> isSuccess o && overflowWarnings o > 0) ) `shouldReturn` True
      it "taskCheck example2' specification with overflow checks" $
        (taskCheckOutcome prog2'' example2' <&> (\o -> isSuccess o && overflowWarnings o == 0) ) `shouldReturn` True


    describe "prog3" $ do
      it "taskCheck example3 specification" $
        (taskCheckOutcome prog3 example3 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example1 specification" $
        (taskCheckOutcome prog3 example1 <&> isFailure) `shouldReturn` True
      it "does not fulfill example2 specification" $
        (taskCheckOutcome prog3 example2 <&> isFailure) `shouldReturn` True

    context "variable merging" $ do
      describe "taskCheck prog5 example5" $
        it "is success" $
          (taskCheckOutcome prog5 example5 <&> isSuccess) `shouldReturn` True
      describe "taskCheck prog7 example7" $
        it "is success" $
          (taskCheckOutcome prog7 example7 <&> isSuccess) `shouldReturn` True
      describe "interpretation of example7" $
        it "satisfies the specification" $
          (taskCheckOutcome (head $ interpret example7) example7 <&> isSuccess) `shouldReturn` True

    context "abort input mode" $ do
      describe "taskCheck prog6 example6" $
        it "is success" $
          (taskCheckOutcome prog6 example6 <&> isSuccess) `shouldReturn` True
      describe "interpretation of example6" $
        it "satisfies the specification" $
          (taskCheckOutcome (head $ interpret example6) example6 <&> isSuccess) `shouldReturn` True


    context "static test generation" $ do
      describe "prog2" $ do
        it "fulfills tests generated from example2 specification" $
          (((\is -> taskCheckOn is prog2 example2) <$> generateStaticTestSuite stdArgs example2) <&> isSuccess) `shouldReturn` True
        it "does not fulfill tests generated from example1 specification" $
          (((\is -> taskCheckOn is prog2 example1) <$> generateStaticTestSuite stdArgs example1) <&> not . isSuccess) `shouldReturn` True
        it "does not fulfill tests generated from example3 specification" $
          (((\is -> taskCheckOn is prog2 example3) <$> generateStaticTestSuite stdArgs example3) <&> not . isSuccess) `shouldReturn` True

    context "interpretation" $ do
      describe "all interpretations of a specification satisfy that specification" $ do
        it "holds for example1" $
          allM (\p -> isSuccess <$> taskCheckOutcome p example1) (interpret example1) `shouldReturn` True
        it "holds for example6" $
          allM (\p -> isSuccess <$> taskCheckOutcome p example6) (interpret example6) `shouldReturn` True


    context "string inputs" $ do
      describe "echoProg" $ do
        it "satisfies echoSpec" $
          (taskCheckOutcome echoProg echoSpec <&> isSuccess) `shouldReturn` True
      describe "reverseProg" $ do
        it "satisfies reverseProg" $
          (taskCheckOutcome reverseProg reverseSpec <&> isSuccess) `shouldReturn` True
      describe "palindromeProg" $ do
        it "satisfies palindromeProg" $
          (taskCheckOutcome palindromeProg palindromeSpec <&> isSuccess) `shouldReturn` True
      describe "pingPongProg" $ do
        it "satisfies pingPongProg" $
          (taskCheckOutcome pingPongProg pingPongSpec <&> isSuccess) `shouldReturn` True

    context "string/newline semantics" $ do
      describe "stringS1" $ do
        it "is satisfied by stringP1" $
          (taskCheckOutcome stringP1 stringS1 <&> isSuccess) `shouldReturn` True
        it "is satisfied by stringP2" $
          (taskCheckOutcome stringP2 stringS1 <&> isSuccess) `shouldReturn` True
        it "is satisfied by stringP3" $
          (taskCheckOutcome stringP3 stringS1 <&> isSuccess) `shouldReturn` True

      describe "stringS2" $ do
        it "is satisfied by stringP1" $
          (taskCheckOutcome stringP1 stringS2 <&> isSuccess) `shouldReturn` True
        it "is not satisfied by stringP2" $
          (taskCheckOutcome stringP2 stringS2 <&> isFailure) `shouldReturn` True
        it "is satisfied by stringP3" $
          (taskCheckOutcome stringP3 stringS2 <&> isSuccess) `shouldReturn` True

  context "random testing" $ do
    describe "prog1" $ do
      it "taskCheck example1 specification" $
        (Random.taskCheckOutcome prog1 example1 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example2 specification" $
        (Random.taskCheckOutcome prog1 example2 <&> isFailure) `shouldReturn` True
      it "does not fulfill example3 specification" $
        (Random.taskCheckOutcome prog1 example3 <&> isFailure) `shouldReturn` True

    describe "prog2" $ do
      it "taskCheck example2 specification" $
        (Random.taskCheckOutcome prog2 example2 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example1 specification" $
        (Random.taskCheckOutcome prog2 example1 <&> isFailure) `shouldReturn` True
      it "does not fulfill example3 specification" $
        (Random.taskCheckOutcome prog2 example3 <&> isFailure) `shouldReturn` True

    describe "prog3" $ do
      it "taskCheck example3 specification" $
        (Random.taskCheckOutcome prog3 example3 <&> isSuccess) `shouldReturn` True
      it "does not fulfill example1 specification" $
        (Random.taskCheckOutcome prog3 example1 <&> isFailure) `shouldReturn` True
      it "does not fulfill example2 specification" $
        (Random.taskCheckOutcome prog3 example2 <&> isFailure) `shouldReturn` True
