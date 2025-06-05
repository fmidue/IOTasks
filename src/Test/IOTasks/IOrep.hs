{-# LANGUAGE DeriveFunctor #-}
module Test.IOTasks.IOrep (
  IOrep , Line,
  runProgram,
  MonadTeletype(..),
  -- ** Re-exports from "System.IO"
  BufferMode(..), stdout,
  ) where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Data.Set (singleton)

import Control.Monad (ap, (>=>))

import Test.IOTasks.Trace
import Test.IOTasks.Internal.OutputPattern
import Test.IOTasks.MonadTeletype

import System.IO (BufferMode(..), stdout) -- for re-exports

data IOrep a
  = GetChar (Char -> IOrep a)
  | PutString String (IOrep a)
  | Return a
  deriving Functor

instance Applicative IOrep where
  (<*>) = ap
  pure = Return

instance Monad IOrep where
  (Return a) >>= g = g a
  (GetChar f) >>= g = GetChar (f >=> g)
  (PutString s ma) >>= g = PutString s (ma >>= g)
  return = pure

instance MonadTeletype IOrep where
  putChar c = PutString [c] $ pure ()
  putStr s = PutString s $ pure ()
  getChar = GetChar pure

type Line = String

runProgram :: IOrep () -> [Line] -> AbstractTrace
runProgram (GetChar _) [] = outOfInputs
runProgram (GetChar f) ("":is) = progRead '\n' <> runProgram (f '\n') is
runProgram (GetChar f) ((c:cs):is) = progRead c <> runProgram (f c) (cs:is)
runProgram (PutString n p') is = progWrite Mandatory (singleton $ Text n) <> runProgram p' is
runProgram (Return ()) _ = terminate
