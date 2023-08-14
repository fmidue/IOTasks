{-# LANGUAGE TypeApplications #-}
module BasicExamplesSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks
import qualified Test.IOTasks.Random as Random

import Control.Monad.Extra(allM)
import Data.Functor((<&>))

-- basic choice between two sets of outputs
spec1 :: Specification
spec1 =
  readInput x ints AssumeValid <>
  readInput y nats AssumeValid <>
  branch (currentValue x .>. as @Integer (currentValue y))
    (writeOutput [wildcard <> value (currentValue x .+. currentValue y) <> wildcard , value (currentValue x .-. currentValue y) <> wildcard] )
    (writeOutput [wildcard <> text "Result: " <> value (currentValue x .*. currentValue y)] )
  where
    x = intVar "x"
    y = intVar "y"

prog1 :: MonadTeletype m => m ()
prog1 = do
  x <- readLn @_ @Integer
  y <- readLn
  if x > y
    then print $ x + y
    else putStr "Result: " >> print (x * y)

-- basic summation
spec2 :: Specification
spec2 =
  readInput n nats AssumeValid <>
  whileNot (sum' (allValues x) .>. currentValue n)
  (readInput x ints AssumeValid) <>
  writeOutput [value $ length' $ as @[Integer] $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

prog2 :: MonadTeletype m => m ()
prog2 = do
  n <- readLn @_ @Integer
  let
    loop s m
      | s > n  = print @_ @Integer m
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

-- summation with loop until first number is non-negativ
-- and also with optional progress output
spec3 :: Specification
spec3 =
  readInput n nats UntilValid <>
  whileNot (length' (as @[Integer] $ allValues x) .==. currentValue n)
    (writeOptionalOutput [value $ currentValue n .-. length' (as @[Integer] $ allValues x)] <> readInput x ints AssumeValid) <>
  writeOutput [value $ sum' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

prog3 :: MonadTeletype m => m ()
prog3 = do
  n <- readLn @_ @Integer
  if n < 0
    then prog3
    else
      let
        loop 0 x = print @_ @Integer x
        loop m x = do
          print m
          i <- readLn
          loop (m-1) (x+i)
      in loop n 0

prog3Wrong :: MonadTeletype m => m ()
prog3Wrong = do
  n <- readLn @_ @Integer
  let
    loop m x
      | m >= n = print @_ @Integer x
      | otherwise = do
        i <- readLn
        loop (m+1) (x+1+i)
  loop 1 0

spec :: Spec
spec = do
  context "solver based testing" $ do
    describe "prog1" $ do
      it "taskCheck spec1 specification" $
        (taskCheckOutcome prog1 spec1 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec2 specification" $
        (taskCheckOutcome prog1 spec2 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec3 specification" $
        (taskCheckOutcome prog1 spec3 <&> isFailure) `shouldReturn` True

    describe "prog3" $ do
      it "taskCheck spec3 specification" $
        (taskCheckOutcome prog3 spec3 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec1 specification" $
        (taskCheckOutcome prog3 spec1 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec2 specification" $
        (taskCheckOutcome prog3 spec2 <&> isFailure) `shouldReturn` True

    describe "prog3Wrong" $ do
      it "taskCheck spec3 specification" $
          (taskCheckOutcome prog3Wrong spec3 <&> isSuccess) `shouldReturn` False

    describe "prog2" $ do
      it "taskCheck spec2 specification" $
        (taskCheckOutcome prog2 spec2 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec1 specification" $
        (taskCheckOutcome prog2 spec1 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec3 specification" $
        (taskCheckOutcome prog2 spec3 <&> isFailure) `shouldReturn` True

  context "static test generation" $ do
    describe "prog3" $ do
      it "fulfills tests generated from spec3 specification" $
        (((\is -> taskCheckOn is prog3 spec3) <$> generateStaticTestSuite stdArgs spec3) <&> isSuccess) `shouldReturn` True
      it "does not fulfill tests generated from spec1 specification" $
        (((\is -> taskCheckOn is prog3 spec1) <$> generateStaticTestSuite stdArgs spec1) <&> not . isSuccess) `shouldReturn` True
      it "does not fulfill tests generated from spec2 specification" $
        (((\is -> taskCheckOn is prog3 spec2) <$> generateStaticTestSuite stdArgs spec2) <&> not . isSuccess) `shouldReturn` True

  context "random testing" $ do
    describe "prog1" $ do
      it "taskCheck spec1 specification" $
        (Random.taskCheckOutcome prog1 spec1 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec2 specification" $
        (Random.taskCheckOutcome prog1 spec2 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec3 specification" $
        (Random.taskCheckOutcome prog1 spec3 <&> isFailure) `shouldReturn` True

    describe "prog3" $ do
      it "taskCheck spec3 specification" $
        (Random.taskCheckOutcome prog3 spec3 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec1 specification" $
        (Random.taskCheckOutcome prog3 spec1 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec2 specification" $
        (Random.taskCheckOutcome prog3 spec2 <&> isFailure) `shouldReturn` True

    describe "prog2" $ do
      it "taskCheck spec2 specification" $
        (Random.taskCheckOutcome prog2 spec2 <&> isSuccess) `shouldReturn` True
      it "does not fulfill spec1 specification" $
        (Random.taskCheckOutcome prog2 spec1 <&> isFailure) `shouldReturn` True
      it "does not fulfill spec3 specification" $
        (Random.taskCheckOutcome prog2 spec3 <&> isFailure) `shouldReturn` True

  context "interpretation" $ do
    describe "all interpretations of a specification satisfy that specification" $ do
      it "holds for spec1" $
        allM (\p -> isSuccess <$> taskCheckOutcome p spec1) (interpret spec1) `shouldReturn` True
      it "holds for spec2" $
        allM (\p -> isSuccess <$> taskCheckOutcome p spec2) (interpret spec2) `shouldReturn` True
      it "holds for spec3" $
        allM (\p -> isSuccess <$> taskCheckOutcome p spec3) (interpret spec3) `shouldReturn` True
