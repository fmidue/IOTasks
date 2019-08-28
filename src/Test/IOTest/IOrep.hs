{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTest.IOrep (
  IOrep,
  runProgram,
  TeletypeM(..),
  Handle, BufferMode, hSetBuffering, stdout,
) where

import Prelude hiding (getLine, putStrLn, print, readLn)

import Test.IOTest.Internal.Trace
import Control.Monad
import qualified System.IO as IO

data IOrep' t a where
  ReadLine :: (t -> IOrep' t a) -> IOrep' t a
  WriteLine :: t -> IOrep' t a -> IOrep' t a
  Return :: a -> IOrep' t a

deriving instance Functor (IOrep' t)

instance Applicative (IOrep' t) where
  (<*>) = ap
  pure = Return

instance Monad (IOrep' t) where
  (Return a) >>= g = g a
  (ReadLine f) >>= g = ReadLine (f >=> g)
  (WriteLine s ma) >>= g = WriteLine s (ma >>= g)
  return = pure

type IOrep = IOrep' String

instance TeletypeM IOrep where
  putStrLn s = WriteLine s $ Return ()
  getLine = ReadLine Return

runProgram :: [String] -> IOrep () -> Trace
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
