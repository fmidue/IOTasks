{-# LANGUAGE TypeApplications #-}
module OverflowSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor((<&>))

-- simple overflow example
overflowSpec :: Specification
overflowSpec =
  readInput n nats UntilValid <>
  whileNot (length' (allValues x) .==. currentValue n)
    (writeOptionalOutput [resultOf $ currentValue n .-. length' (allValues x)] <> readInput x ints AssumeValid) <>
  writeOutput [resultOf $ product' $ allValues x]
  where
    n = intVar "n"
    x = intVar "x"

overflowProg :: MonadTeletype m => m ()
overflowProg = do
  n <- readLn @_ @Integer
  if n < 0
    then overflowProg
    else
      let
      loop 0 x = print @_ @Integer x
      loop m x = do
        print m
        i <- readLn
        loop (m-1) (x*i)
      in loop n 1

spec :: Spec
spec =
  describe "overflowProg" $ do
    it "taskCheck overflowSpec specification with overflow warnings" $
      (taskCheckWithOutcome stdArgs{avoidOverflows=False} overflowProg overflowSpec <&> (\o -> isSuccess o && overflowWarnings o > 0) ) `shouldReturn` True
    it "taskCheck overflowSpec specification with overflow checks" $
      (taskCheckOutcome overflowProg overflowSpec <&> (\o -> isSuccess o && overflowWarnings o == 0) ) `shouldReturn` True
