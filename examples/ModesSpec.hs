{-# LANGUAGE TypeApplications #-}
module ModesSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Control.Monad (when)
import Control.Monad.Extra(allM, replicateM_)
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

--
spec2 :: InputMode -> Specification
spec2 mode =
  readInput x nats mode `repeatUntil` (length' (as @[Integer] $ allValues x) .==. intLit 5)
  where
    x = intVar "x"

prog2 :: MonadTeletype io => io Integer -> io ()
prog2 = replicateM_ 5

prog2Abort :: MonadTeletype io => io ()
prog2Abort = do
  loop 5
  where
    loop 0 = pure ()
    loop n = do
      x <- readLn
      when (x >= 0) $ loop (n-1)

readValid, readUntil :: MonadTeletype io => io Integer
readValid = readLn
readUntil = do
  n <- readLn
  if n >= 0 then pure n else readUntil


spec :: Spec
spec = do
  context "abort input mode" $ do
    describe "taskCheck program specification" $
      it "is success" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True
    describe "interpretation of specification" $
      it "satisfies the specification" $
        (taskCheckOutcome (head $ interpret specification) specification <&> isSuccess) `shouldReturn` True

  context "relationship between input modes" $ do
    describe "prog2 readValid" $ do
      it "should satisfy 'spec2 AssumeValid'" $  (taskCheckOutcome (prog2 readValid) (spec2 AssumeValid)  <&> isSuccess) `shouldReturn` True
      it "should not satisfy 'spec2 UntilValid'" $  (taskCheckOutcome (prog2 readValid) (spec2 UntilValid)  <&> isSuccess) `shouldReturn` False
      it "should not satisfy 'spec2 ElseAbort'" $  (taskCheckOutcome (prog2 readValid) (spec2 ElseAbort)  <&> isSuccess) `shouldReturn` False
    describe "prog2 readUntil" $ do
      it "should satisfy 'spec2 UntilValid'" $  (taskCheckOutcome (prog2 readUntil) (spec2 UntilValid)  <&> isSuccess) `shouldReturn` True
      it "should satisfy 'spec2 AssumeValid'" $  (taskCheckOutcome (prog2 readUntil) (spec2 AssumeValid)  <&> isSuccess) `shouldReturn` True
      it "should not satisfy 'spec2 ElseAbort'" $  (taskCheckOutcome (prog2 readUntil) (spec2 ElseAbort)  <&> isSuccess) `shouldReturn` False
    describe "prog2Abort" $ do
      it "should satisfy 'spec2 ElseAbort'" $  (taskCheckOutcome prog2Abort (spec2 ElseAbort)  <&> isSuccess) `shouldReturn` True
      it "should satisfy 'spec2 AssumeValid'" $  (taskCheckOutcome prog2Abort (spec2 AssumeValid)  <&> isSuccess) `shouldReturn` True
      it "should not satisfy 'spec2 UntilValid'" $  (taskCheckOutcome prog2Abort (spec2 UntilValid)  <&> isSuccess) `shouldReturn` False

  context "interpretation" $
    describe "all interpretations of a specification satisfy that specification" $
      it "holds for specification" $
        allM (\p -> isSuccess <$> taskCheckOutcome p specification) (interpret specification) `shouldReturn` True
