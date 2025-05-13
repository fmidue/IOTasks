{-# LANGUAGE TypeApplications #-}
module NonEmptyWildcardSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))
import Test.IOTasks.OutputPattern (nonEmptyWildcard)

simplePrintSpec :: Specification
simplePrintSpec =
  writeOutput [nonEmptyWildcard]

simplePrint :: MonadTeletype m => m ()
simplePrint = putStrLn "ABC"

emptyPrint :: MonadTeletype m => m ()
emptyPrint = putStrLn ""

decoratedSpec :: Specification
decoratedSpec =
  readInput x ints AssumeValid <>
  readInput x ints AssumeValid <>
  readInput x ints AssumeValid <>
  writeOutput [decoratedResultOf (sum' $ allValues x)]
  where
    x = intVar "x"

read3Values :: MonadTeletype m => m (Int,Int,Int)
read3Values = do
  x <- readLn
  y <- readLn
  z <- readLn
  pure $ (x,y,z)

printDecoration1 :: MonadTeletype m => m ()
printDecoration1 = do
  (x,y,z) <- read3Values
  putStrLn $ "sum: " <> show (x+y+z)

printDecoration2 :: MonadTeletype m => m ()
printDecoration2 = do
  (x,y,z) <- read3Values
  putStrLn $ show (x+y+z) <> " is the sum of the inputs"

printPlainResult :: MonadTeletype m => m ()
printPlainResult = do
  (x,y,z) <- read3Values
  print $ x+y+z

spec :: Spec
spec =
  context "non-empty wildcards" $ do
    describe "simplePrintSpec" $ do
      it "accepts simplePrint" $
        (taskCheckOutcome simplePrint simplePrintSpec <&> isSuccess) `shouldReturn` True
      it "rejects emptyPrint" $
        (taskCheckOutcome emptyPrint simplePrintSpec <&> isSuccess) `shouldReturn` False
    describe "decoratedSpec" $ do
      it "accepts printDecoration1" $
        (taskCheckOutcome printDecoration1 decoratedSpec <&> isSuccess) `shouldReturn` True
      it "accepts printDecoration2" $
        (taskCheckOutcome printDecoration2 decoratedSpec <&> isSuccess) `shouldReturn` True
      it "rejects printPlainResult" $
        (taskCheckOutcome printPlainResult decoratedSpec <&> isSuccess) `shouldReturn` False
