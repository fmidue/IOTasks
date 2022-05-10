{-# LANGUAGE DeriveFunctor #-}
module IOrep where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Data.Set (singleton)

import Control.Monad (ap, (>=>))

import Trace
import OutputPattern

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

putChar :: Char -> IOrep ()
putChar c = putString [c] $ pure ()

putStr :: String -> IOrep ()
putStr s = putString s $ pure ()

putStrLn :: String -> IOrep ()
putStrLn s = putStr $ s++"\n"

print :: Show a => a -> IOrep ()
print = putStrLn . show

getChar :: IOrep Char
getChar = GetChar pure

getLine :: IOrep String
getLine = do
  c <- getChar
  case c of
    '\n' -> pure [c]
    _ -> (c:) <$> getLine

readLn :: Read a => IOrep a
readLn = read <$> getLine

type Line = String

runProgram :: [Line] -> IOrep () -> Trace
runProgram [] (GetChar _) = OutOfInputs
runProgram ("":is) (GetChar f) = ProgRead '\n' $ runProgram is (f '\n')
runProgram ((c:cs):is) (GetChar f) = ProgRead c $ runProgram (cs:is) (f c)
runProgram is (PutString n p') = ProgWrite Mandatory (singleton $ Text n) $ runProgram is p'
runProgram _ (Return ()) = Terminate
