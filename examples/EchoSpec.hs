{-# LANGUAGE TypeApplications #-}
module EchoSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

echoSpec :: Specification
echoSpec =
  readInput x str AssumeValid <>
  writeOutput [resultOf $ as @String $ currentValue x]
  where
    x = stringVar "x"

echoProg :: MonadTeletype m => m ()
echoProg = getLine >>= putStrLn

spec :: Spec
spec =
  context "string inputs" $ do
    describe "echoProg" $ do
      it "satisfies echoSpec" $
        (taskCheckOutcome echoProg echoSpec <&> isSuccess) `shouldReturn` True
