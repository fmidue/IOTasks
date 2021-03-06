{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Test.IOTasks.IOrep (
  IOrep(..),
  runProgram,
  MonadTeletype(..), print, readLn,
  IO.BufferMode(NoBuffering),
  IO.stdout,
  Exit(..),
) where

import Prelude hiding (getLine, putStrLn, print, readLn, putChar, putStr)

import Test.IOTasks.Trace
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.State
import qualified System.IO as IO

data IOrep a
  = GetLine (String -> IOrep a)
  | PutChar Char (IOrep a)
  | Return a
  deriving Functor

instance Applicative IOrep where
  (<*>) = ap
  pure = Return

instance Monad IOrep where
  (Return a) >>= g = g a
  (GetLine f) >>= g = GetLine (f >=> g)
  (PutChar s ma) >>= g = PutChar s (ma >>= g)
  return = pure

instance MonadTeletype IOrep where
  putChar c = PutChar c $ Return ()
  getLine = GetLine Return

  hSetBuffering _ IO.NoBuffering = pure ()
  hSetBuffering _ m = error $ "hSetBuffering unsupported buffering mode: " ++ show m

-- returns Nothing if the number of inputs provided was not sufficient to complete the program run
runProgram :: [String] -> IOrep () -> OrdinaryTrace
runProgram is
  | any ('\n' `elem`) is = error "runProgram: each input has to be a single line"
  | otherwise = go Nothing is where
  -- acc: accumulates writes
  go :: Maybe String -> [String] -> IOrep () -> OrdinaryTrace
  go Nothing (x:xs) (GetLine f) = ProgRead x $ go Nothing xs (f x)
  go (Just acc) (x:xs) (GetLine f) = ProgWrite (reverse acc) $ ProgRead x $ go Nothing xs (f x)

  go Nothing [] (GetLine _) = ProgRead "<unknown input>" undefined
  go (Just acc) [] (GetLine _) = ProgWrite (reverse acc) $ ProgRead "<unknown input>" undefined

  go Nothing xs (PutChar '\n' p) = ProgWrite "\n" $ go Nothing xs p
  go Nothing xs (PutChar v p) = go (Just [v]) xs p
  go (Just acc) xs (PutChar '\n' p) = ProgWrite (reverse ('\n':acc)) $ go Nothing xs p
  go (Just acc) xs (PutChar v p) = go (Just $ v:acc) xs p

  go Nothing _ (Return _) = Stop
  go (Just acc) _ (Return _) = ProgWrite (reverse acc) Stop

class Monad m => MonadTeletype m where
  putChar :: Char -> m ()
  getLine :: m String

  {- HLINT ignore putStr -}
  putStr :: String -> m ()
  putStr = mapM_ putChar

  putStrLn :: String -> m ()
  putStrLn s = putStr s >> putChar '\n'

  hSetBuffering :: IO.Handle -> IO.BufferMode -> m ()

print :: (Show a, MonadTeletype m) => a -> m ()
print = putStrLn . show

readLn :: (Read a, MonadTeletype m) => m a
readLn = read <$> getLine

data Exit = Exit

instance MonadTeletype m => MonadTeletype (ExceptT Exit m) where
  putChar = lift . putChar
  getLine = lift getLine
  hSetBuffering h = lift . hSetBuffering h


instance MonadTeletype m => MonadTeletype (StateT s m) where
  putChar = lift . putChar
  getLine = lift getLine
  hSetBuffering h = lift . hSetBuffering h

instance MonadTeletype IO where
  putChar = IO.putChar
  putStr = IO.putStr
  putStrLn = IO.putStrLn
  getLine = IO.getLine
  hSetBuffering = IO.hSetBuffering
