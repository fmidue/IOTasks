{-# LANGUAGE TypeApplications #-}
module SymbolicPlusSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))
import Data.Function (on)
import Data.List (sortBy)

addSpec :: Specification
addSpec =
  readInput x nats AssumeValid <>
  readInput y nats AssumeValid <>
  writeOutput [value $ currentValue x .+. currentValue y]
  where
    x = intVar "x"
    y = intVar "y"

program :: MonadTeletype m => m ()
program = do
  putStrLn =<< plus <$> getLine <*> getLine

plus :: String -> String -> String
plus x y = reverse $ plus' (reverse $ filter (>= '0') x') (reverse $ filter (>= '0') y') where
  ~[x',y'] = sortBy (compare `on` length) [x,y]
  plus' xs [] = xs
  plus' [] ys = ys
  plus' ('0':xs) (y:ys) = y : plus' xs ys
  plus' [x] ('9':ys) = plus' (pred x:['1']) ('0':ys)
  plus' (x:x':xs) ('9':ys) = plus' (pred x:succ x':xs) ('0':ys)
  plus' (x:xs) (y:ys) = plus' (pred x:xs) (succ y:ys)

spec :: Spec
spec =
  describe "taskCheck program addSpec" $
    it "succeeds" $
      (taskCheckWithOutcome stdArgs{maxSuccessPerPath = 100, valueSize = 1000} program addSpec <&> isSuccess) `shouldReturn` True
