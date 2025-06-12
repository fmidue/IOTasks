{-# LANGUAGE TypeApplications #-}
module SumToZeroRegressionSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))
import Test.IOTasks.Trace (isOutputMismatch)

verboseSpec :: Specification
verboseSpec =
  readX <>
  whileNot (currentValue x .==. intLit 0)
    (readY <>
     writeOutput [wildcard <> resultOf (currentValue x .+. currentValue y) <> wildcard] <>
     readX
    ) <>
  writeOutput [wildcard <> resultOf (length' $ allValues y) <> wildcard]
  where
    readX =
      anyOptionalOutput <>
      readInput x ints AssumeValid

    readY =
      anyOptionalOutput <>
      readInput y ints AssumeValid

    x = intVar "x"
    y = intVar "y"

verboseWrongProg :: MonadTeletype io => io ()
verboseWrongProg = go 0
  where
    go :: MonadTeletype io => Integer -> io ()
    go n = do
      putStr "First number or 0 to exit: "
      x <- readLn
      if x == 0
        then do
          putStrLn "Exiting program"
          putStr "The number of additions performed was: "
          print n
        else do
          putStr "Second number: "
          y <- readLn
          putStr ("The sum of " ++ show x ++ " and " ++ show y ++ " is: ")
          print (x + y + 1)
          go (n + 1)

spec :: Spec
spec = do
  describe "taskCheck verboseWrongProg verboseSpec" $ do
    it "should be rejected with an output mismatch" $
      (taskCheckOutcome verboseWrongProg verboseSpec <&> isOutputMismatchOutcome) `shouldReturn` True

isOutputMismatchOutcome :: Outcome -> Bool
isOutputMismatchOutcome (Outcome (Failure _ _ _ match)  _) = isOutputMismatch match
isOutputMismatchOutcome _ = False

