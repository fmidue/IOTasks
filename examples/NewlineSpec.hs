{-# LANGUAGE TypeApplications #-}
module NewlineSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor((<&>))

stringS1 :: Specification
stringS1 = writeOutput [text "A"] <> writeOutput [text "B"]

stringS2 :: Specification
stringS2 = writeOutput [text "A" <> text "B"]

stringP1 :: MonadTeletype m => m ()
stringP1 = putStrLn "AB"

stringP2 :: MonadTeletype m => m ()
stringP2 = putStrLn "A" >> putStrLn "B"

stringP3 :: MonadTeletype m => m ()
stringP3 = putStr "A" >> putStr "B"

spec :: Spec
spec =
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
