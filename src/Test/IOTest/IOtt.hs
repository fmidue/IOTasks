{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTest.IOtt (
  IOtt,
  runProgram,
  TeletypeM(..),
  Handle, BufferMode, hSetBuffering, stdout,
) where

import Prelude hiding (getLine, putStrLn, print, readLn)

import Test.IOTest.Internal.Trace
import Control.Monad
import qualified System.IO as IO

data IOtt' t a where
  ReadLine :: (t -> IOtt' t a) -> IOtt' t a
  WriteLine :: t -> IOtt' t a -> IOtt' t a
  Return :: a -> IOtt' t a

deriving instance Functor (IOtt' t)

instance Applicative (IOtt' t) where
  (<*>) = ap
  pure = Return

instance Monad (IOtt' t) where
  (Return a) >>= g = g a
  (ReadLine f) >>= g = ReadLine (f >=> g)
  (WriteLine s ma) >>= g = WriteLine s (ma >>= g)
  return = pure

type IOtt = IOtt' String

instance TeletypeM IOtt where
  putStrLn s = WriteLine s $ Return ()
  getLine = ReadLine Return

runProgram :: [String] -> IOtt () -> Trace
runProgram (x:xs) (ReadLine f) = ProgRead x $ runProgram xs $ f x
runProgram [] (ReadLine _) = OutOfInputs
runProgram xs (WriteLine v p) =  ProgWrite v $ runProgram xs p
runProgram _ (Return _) =  Stop

class Monad m => TeletypeM m where
  putStrLn :: String -> m ()

  getLine :: m String

  print :: Show a => a -> m ()
  print = putStrLn . show

  readLn :: Read a => m a
  readLn = read <$> getLine

data Handle = StdOut

stdout :: Handle
stdout = StdOut

data BufferMode = NoBuffering

hSetBuffering :: TeletypeM m => Handle -> BufferMode -> m ()
hSetBuffering StdOut NoBuffering = return ()

instance TeletypeM IO where
  putStrLn = IO.putStrLn
  getLine = IO.getLine
  readLn = IO.readLn
  print = IO.print
