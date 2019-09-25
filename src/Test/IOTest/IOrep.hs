{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTest.IOrep (
  IOrep,
  runProgram,
  MonadTeletype(..), print, readLn,
  Handle, BufferMode, hSetBuffering, stdout,
  Exit(..),
) where

import Prelude hiding (getLine, putStrLn, print, readLn)

import Test.IOTest.Internal.Trace
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified System.IO as IO

data IOrep' t a where
  GetLine :: (t -> IOrep' t a) -> IOrep' t a
  PutLine :: t -> IOrep' t a -> IOrep' t a
  Return :: a -> IOrep' t a

deriving instance Functor (IOrep' t)

instance Applicative (IOrep' t) where
  (<*>) = ap
  pure = Return

instance Monad (IOrep' t) where
  (Return a) >>= g = g a
  (GetLine f) >>= g = GetLine (f >=> g)
  (PutLine s ma) >>= g = PutLine s (ma >>= g)
  return = pure

type IOrep = IOrep' String

instance MonadTeletype IOrep where
  putStrLn s = PutLine s $ Return ()
  getLine = GetLine Return

runProgram :: [String] -> IOrep () -> Trace
runProgram (x:xs) (GetLine f) = Trace [ProgRead x] <> runProgram xs (f x)
runProgram [] (GetLine _) = Trace [OutOfInputs]
runProgram xs (PutLine v p) =  Trace [ProgWrite v] <> runProgram xs p
runProgram _ (Return _) =  Trace []

class Monad m => MonadTeletype m where
  putStrLn :: String -> m ()
  getLine :: m String

print :: (Show a, MonadTeletype m) => a -> m ()
print = putStrLn . show

readLn :: (Read a, MonadTeletype m) => m a
readLn = read <$> getLine

data Exit = Exit

instance MonadTeletype m => MonadTeletype (ExceptT Exit m) where
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
