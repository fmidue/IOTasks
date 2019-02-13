{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module IOtt where

import Prelude hiding (getLine, putStrLn, print)

import Trace
import Control.Monad


data IOtt' t a where
  ReadLine :: (t -> IOtt' t a) -> IOtt' t a
  WriteLine :: t -> IOtt' t a -> IOtt' t a
  Return :: a -> IOtt' t a

instance Show (IOtt' t a) where
  show _ = "TODO: implement Show (IOtt' t a)"

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

runProgram :: [t] -> IOtt' t () -> Trace' t
runProgram (x:xs) (ReadLine f) = ProgRead x $ runProgram xs $ f x
runProgram [] (ReadLine _) = OutOfInputs
runProgram xs (WriteLine v p) =  ProgWrite [v] $ runProgram xs p
runProgram _ (Return _) =  Stop
