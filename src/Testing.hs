{-# LANGUAGE TypeApplications #-}
module Testing where

import IOtt (IOtt, Trace', changeCarrier, runtt)
import Type
import Matching

import Data.Either
import Control.Monad.Free
import Control.Monad
import Control.Monad.Trans.Writer

import Test.QuickCheck

test :: IOtt () -> Spec VarName -> IO Bool
test program spec = do
  result <- dropWhile (isRight . fst) <$> replicateM 100 (testRun program spec)
  case result of
    [] -> putStrLn "++ 100 tests passed ++"
    ((Left x,is):_) -> putStrLn $ "++ Failed: " ++ x ++ "\n++ Input to reproduce: " ++ show is
    ((Right _,_):_) -> error "impossible"
  return $ null result

testRun :: IOtt () -> Spec VarName -> IO (Either String (), [Int])
testRun prog spec = runWriterT (matchesPartial (choose (-10,10)) (Pure prog) spec)

specProperty :: Spec VarName -> IOtt () -> Property
specProperty spec program =
  let gen = inputGenerator spec
      prop inputs = let trace = runtt program (inputs ++ ["0"])
                    in testTrace (changeCarrier read trace) spec
  in forAll gen prop

testTrace :: Trace' Int () -> Spec VarName -> Property
testTrace trace spec =
  let res = matchesFull trace spec
      msg = fromLeft "" res
  in counterexample msg $ isRight res

inputGenerator :: Spec a -> Gen [String]
inputGenerator _s = vectorOf 11 $ show <$> choose @Int (1,10)
