{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTest.IOrep (
  IOrep,
  runProgram,
  MonadTeletype(..),
  Handle, BufferMode, hSetBuffering, stdout,
) where

import Prelude hiding (getLine, putStrLn, print, readLn)

import Test.IOTest.Internal.Trace
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
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

instance MonadTeletype IOrep where
  putStrLn s = WriteLine s $ Return ()
  getLine = ReadLine Return

runProgram :: [String] -> IOrep () -> Trace
runProgram (x:xs) (ReadLine f) = Trace [ProgRead x] <> runProgram xs (f x)
runProgram [] (ReadLine _) = Trace [OutOfInputs]
runProgram xs (WriteLine v p) =  Trace [ProgWrite v] <> runProgram xs p
runProgram _ (Return _) =  Trace []

class Monad m => MonadTeletype m where
  putStrLn :: String -> m ()

  getLine :: m String

  print :: Show a => a -> m ()
  print = putStrLn . show

  readLn :: Read a => m a
  readLn = read <$> getLine

instance MonadTeletype m => MonadTeletype (MaybeT m) where
  putStrLn = lift . putStrLn
  getLine = lift getLine

instance MonadTeletype m => MonadTeletype (StateT s m) where
  putStrLn = lift . putStrLn
  getLine = lift getLine

data Handle = StdOut

stdout :: Handle
stdout = StdOut

data BufferMode = NoBuffering

hSetBuffering :: MonadTeletype m => Handle -> BufferMode -> m ()
hSetBuffering StdOut NoBuffering = return ()

instance MonadTeletype IO where
  putStrLn = IO.putStrLn
  getLine = IO.getLine
  readLn = IO.readLn
  print = IO.print
