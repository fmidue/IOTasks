{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module IOtt where

import Prelude hiding (getLine, putStrLn, print)

import Control.Monad ((>=>),ap)

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

getLine :: IOtt String
getLine = ReadLine Return

putStrLn :: String -> IOtt ()
putStrLn s = WriteLine s $ Return ()

print :: Show a => a -> IOtt ()
print = putStrLn . show

maybePrint :: Show a => Maybe a -> IOtt ()
maybePrint Nothing = return ()
maybePrint (Just xs) = print xs

type Output = Output' String

data Output' t a
  = Read (Output' t a)
  | Write t (Output' t a)
  | Finish a
  | OutOfInputs
  deriving (Eq, Show)

runtt :: IOtt' t a -> [t] -> Output' t a
runtt (Return a) _ = Finish a
runtt (ReadLine f) (x:xs) = Read (runtt (f x) xs)
runtt (ReadLine _) [] = OutOfInputs
runtt (WriteLine x p) xs = Write x $ runtt p xs
