{-# LANGUAGE TypeApplications #-}
module ReverseSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

reverseSpec :: Specification
reverseSpec =
  readInput x str AssumeValid <>
  branch (length' (currentValue x) .>. intLit 5)
    (writeOutput [resultOf $ reverse' $ currentValue x])
    (writeOutput [resultOf $ currentValue x])
  where
    x = stringVar "x"

reverseProg :: MonadTeletype m => m ()
reverseProg = do
  str <- getLine
  if length str > 5
    then putStrLn $ reverse str
    else putStrLn str

spec :: Spec
spec =
  context "string inputs" $ do
    describe "reverseProg" $ do
      it "satisfies reverseProg" $
          (taskCheckOutcome reverseProg reverseSpec <&> isSuccess) `shouldReturn` True
