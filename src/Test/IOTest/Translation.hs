{-# LANGUAGE GADTs #-}
module Test.IOTest.Translation (
  buildProgram
) where

import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term hiding (update)
import Test.IOTest.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.Internal.Specification
import Test.IOTest.IOtt

import Control.Monad (void)

buildProgram :: StringRep a => Specification VarName a -> IOtt ()
buildProgram s = void $ translate s (freshContext s)

translate :: StringRep a => Specification VarName a -> Context VarName a -> IOtt (Context VarName a,LoopEnd)
translate (ReadInput x _) d = do
  v <- from <$> getLine
  return (update d x v, No)
translate (WriteOutput []) _ = error "empty list of output options"
translate (WriteOutput (f: _)) d | isEpsilon f = return (d,No)
translate (WriteOutput (f:_)) d = do
  putStrLn . to $ evalTerm f d
  return (d, No)
translate (WriteOutputP []) _ = error "empty pattern set"
translate (WriteOutputP (NoOutput : _ )) d = return (d,No)
translate (WriteOutputP (Exactly t _ : _)) d = do
  putStrLn $ show (evalTerm t d)
  return (d, No)
translate (WriteOutputP (Contains t _ : _)) d = do
  putStrLn $ "<some string containing '" ++ show (evalTerm t d) ++ "'>"
  return (d, No)
translate (WriteOutputP (Everything : _)) d = putStrLn "<some string>" >> return (d, No)
translate E d = return (d, Yes)
translate Nop d = return (d, No)
translate (TillE s) d =
  let body = translate s
      go d' = do
        (d'', end) <- body d'
        case end of
          Yes -> return (d'', No)
          No -> go d''
  in go d
translate (Branch p s1 s2) d =
  if evalTerm p d
    then translate s2 d
    else translate s1 d
translate (s1 :<> s2) d = translate s1 d >>= (\(d',_) -> translate s2 d')

data LoopEnd = Yes | No
