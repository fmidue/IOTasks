{-# LANGUAGE TypeApplications #-}
module VariableMerging1Spec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

-- variable merging and multiple exits
specification :: Specification
specification =
  anyOptionalOutput <>
  readInput x ints AssumeValid <>
  anyOptionalOutput <>
  readInput y ints AssumeValid <>
  tillExit (
    branch (currentValue x .+. currentValue y .==. intLit 0 )
      exit
      (anyOptionalOutput <>
       readInput x ints AssumeValid <>
        branch (currentValue x .+. currentValue y .==. intLit 0 )
          exit
          (anyOptionalOutput <>
           readInput y ints AssumeValid)
    )
  ) <>
  writeOutput [wildcard <> resultOf (length' $ filter' predicate $ allValues $ merge [x,y]) <> wildcard]
  where
    predicate x = x > 0 && x `mod` 3 == 0
    x = intVar "x"
    y = intVar "y"

program :: MonadTeletype m => m ()
program = do
  putStr "Please enter a number: "
  n <- readLn
  loop [n]
  where
    loop :: MonadTeletype m => [Integer] -> m ()
    loop (m:ms) = do putStr "Next number please: "
                     n <- readLn
                     if n+m == 0
                       then do putStrLn "The sum of the last two inputs was 0."
                               putStr "Number of positive inputs divisible by three: "
                               print $ length $ filter (\n' -> (n' > 0) && (n' `mod` 3 == 0)) (n:m:ms)
                               putStrLn "Program finished."
                       else do putStrLn "The sum was not 0 yet."
                               loop (n:m:ms)
    loop [] = error "does not happen"


spec :: Spec
spec =
  context "variable merging" $ do
    describe "taskCheck program specification" $
      it "is success" $ (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True
