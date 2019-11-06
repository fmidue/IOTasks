{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.IOrep (
  IOrep,
  runProgram,
  MonadTeletype(..), print, readLn,
  Exit(..),
) where

import Prelude hiding (getLine, putStrLn, print, readLn)

import Test.IOTest.Trace
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified System.IO as IO

data IOrep' t a
  = GetLine (t -> IOrep' t a)
  | PutLine t (IOrep' t a)
  | Return a
  deriving Functor

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

-- returns Nothing if the number of inputs provided was not sufficient to complete the program run
runProgram :: [String] -> IOrep () -> Maybe OrdinaryTrace
runProgram (x:xs) (GetLine f) = ProgRead x <$> runProgram xs (f x)
runProgram [] (GetLine _) = Nothing
runProgram xs (PutLine v p) =  ProgWrite v <$> runProgram xs p
runProgram _ (Return _) =  Just Stop

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

instance MonadTeletype IO where
  putStrLn = IO.putStrLn
  getLine = IO.getLine
