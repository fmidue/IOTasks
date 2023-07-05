{-# LANGUAGE DeriveFunctor #-}
module Test.IOTasks.IOrep
  ( IOrep , Line
  , runProgram
  , MonadTeletype(..), BufferMode(..), stdout
  ) where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Data.Set (singleton)

import Control.Monad (ap, (>=>))

import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern
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
  (PutString s ma) >>= g = putString s (ma >>= g)
  return = pure

putString :: String -> IOrep a -> IOrep a
putString s (PutString s' m) = PutString (s++s') m
putString s m = PutString s m

instance MonadTeletype IOrep where
  putChar c = putString [c] $ pure ()
  putStr s = putString s $ pure ()
  getChar = GetChar pure

type Line = String

runProgram :: [Line] -> IOrep () -> NTrace
runProgram [] (GetChar _) = NOutOfInputs
runProgram ("":is) (GetChar f) = NProgRead '\n' $ runProgram is (f '\n')
runProgram ((c:cs):is) (GetChar f) = NProgRead c $ runProgram (cs:is) (f c)
runProgram is (PutString n p') = NProgWrite Mandatory (singleton $ Text n) $ runProgram is p'
runProgram _ (Return ()) = NTerminate
