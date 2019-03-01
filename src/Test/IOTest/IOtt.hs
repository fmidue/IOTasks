{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Test.IOTest.IOtt (
  IOtt,
  getLine,
  putStrLn,
  print,
  runProgram
) where

import Prelude hiding (getLine, putStrLn, print)

import Test.IOTest.Trace
import Control.Monad

data IOtt' t a where
  ReadLine :: (t -> IOtt' t a) -> IOtt' t a
  WriteLine :: t -> IOtt' t a -> IOtt' t a
  Return :: a -> IOtt' t a

instance (Show a, Show t) => Show (IOtt' t a) where
  show (ReadLine _) = "ReadLine <<func>>"
  show (WriteLine t r) = "WriteLine (" ++ show t ++ show r ++ ")"
  show (Return a) = "Return " ++ show a

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

getLine :: IOtt String
getLine = ReadLine Return

putStrLn :: String -> IOtt ()
putStrLn s = WriteLine s $ Return ()

print :: Show a => a -> IOtt ()
print = putStrLn . show

runProgram :: [t] -> IOtt' t () -> Trace' t t
runProgram (x:xs) (ReadLine f) = ProgRead x $ runProgram xs $ f x
runProgram [] (ReadLine _) = OutOfInputs
runProgram xs (WriteLine v p) =  ProgWrite v $ runProgram xs p
runProgram _ (Return _) =  Stop
