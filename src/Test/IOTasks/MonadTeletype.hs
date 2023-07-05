module Test.IOTasks.MonadTeletype (
  MonadTeletype(..),
  ) where

import Prelude hiding (putChar, putStr, putStrLn, print, getChar, getLine, readLn)
import qualified Prelude

import System.IO (hSetBuffering, Handle, BufferMode)

class Monad m => MonadTeletype m where
  putChar :: Char -> m ()
  getChar :: m Char

  putStr :: String -> m ()
  putStr = mapM_ putChar
  putStrLn :: String -> m ()
  putStrLn = putStr . (++"\n")
  print :: Show a => a -> m ()
  print = putStrLn . show

  getLine :: m String
  getLine = do
    c <- getChar
    case c of
      '\n' -> pure ""
      _ -> (c:) <$> getLine

  readLn :: Read a => m a
  readLn = read <$> getLine

  hSetBuffering :: Handle -> BufferMode -> m ()
  hSetBuffering _ _ = pure ()

instance MonadTeletype IO where
  putChar = Prelude.putChar
  getChar = Prelude.getChar

  putStr = Prelude.putStr
  putStrLn = Prelude.putStrLn
  print = Prelude.print

  getLine = Prelude.getLine
  readLn = Prelude.readLn

  hSetBuffering = System.IO.hSetBuffering
