{-# LANGUAGE TypeApplications #-}
module BlowupSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

specification :: Specification
specification = tillExit $
  branch (sum' (allValues x) .>. intLit 0)
    exit
    ( readInput x ints AssumeValid <>
      branch (currentValue x .>. intLit 0)
        (writeOutput [text "1"])
        (writeOutput [text "0"])
    )
  where
    x = intVar "x"

program :: MonadTeletype io => io ()
program = loop [] where
  loop xs =
    if sum xs > 0
      then pure ()
      else do
        x <- readLn @_ @Integer
        putStrLn $ if x > 0 then "1" else "0"
        loop (x:xs)

spec :: Spec
spec =
  describe "taskCheck program specification" $
    it "succeeds for small unfolding parameter" $
      (taskCheckWithOutcome stdArgs{maxIterationUnfold = 7} program specification <&> isSuccess) `shouldReturn` True
