{-# LANGUAGE DeriveFunctor #-}
module Test.IOTasks.IOrep (
  IOrep , Line,
  runProgram,
  runProgramClean,
  MonadTeletype(..),
  -- ** Re-exports from "System.IO"
  BufferMode(..), stdout,
  ) where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Control.Monad (ap, (>=>))

import Test.IOTasks.Trace.Class
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

runProgram :: Trace t => IOrep () -> [Line] -> t
runProgram = run' id

-- runProgramClean p (inputSequence t) = t
-- however, in general inputSequence (runProgramClean p is) = is does not hold
-- (it still holds for programs without getChar -- including not hidden in any (external) definitions)

-- | Run the program, but make write actions implicitly also read an implicit newline character if directly preceding reads have left exactly that newline character as the first element of the remaining input sequence.
--
-- This makes it so that programs like @getChar >> putStrLn "" >> getLine@ behave identical for input sequences @[char:line]@ and @[[char],line]@ allowing the use of the more intuitive second input sequence.
runProgramClean :: Trace t => IOrep () -> [Line] -> t
runProgramClean = run' cleanInputs
  where
      cleanInputs :: [[a]] -> [[a]]
      cleanInputs ([]:xs) = xs
      cleanInputs xs = xs

run' :: Trace t => ([Line] -> [Line]) -> IOrep () -> [Line] -> t
run' clean (GetChar _) [] = outOfInputs
run' clean (GetChar f) ("":is) = progReadChar '\n' $ run' clean (f '\n') is
run' clean (GetChar f) ((c:cs):is) = progReadChar c $ run' clean (f c) (cs:is)
run' clean (PutString s p') is = progWriteStr s $ run' clean p' (clean is)
run' _     (Return ()) _ = terminate
