{-# LANGUAGE TypeApplications #-}
module ModesSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Control.Monad.Extra(allM)
import Data.Functor ((<&>))

-- input modes
specification :: Specification
specification =
  readInput x nats ElseAbort <>
  readInput y nats AssumeValid <>
  readInput z nats UntilValid <>
  writeOutput [resultOf $ sum' $ allValues [x,y,z] ]
  where
    x = intVar "x"
    y = intVar "y"
    z = intVar "z"

program :: MonadTeletype m => m ()
program = do
  x <- readLn @_ @Integer
  if x < 0
    then putStrLn "abort: invalid input value"
    else do
      y <- readLn
      let loop = do
            v <- readLn
            if v < 0
              then do
                putStrLn "invalid input value, try again"
                loop
              else pure v
      z <- loop
      print $ x + y + z

spec :: Spec
spec = do
  context "abort input mode" $ do
    describe "taskCheck program specification" $
      it "is success" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True
    describe "interpretation of specification" $
      it "satisfies the specification" $
        (taskCheckOutcome (head $ interpret specification) specification <&> isSuccess) `shouldReturn` True

  context "interpretation" $
    describe "all interpretations of a specification satisfy that specification" $
      it "holds for specification" $
        allM (\p -> isSuccess <$> taskCheckOutcome p specification) (interpret specification) `shouldReturn` True
