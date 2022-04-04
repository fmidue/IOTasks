{-# LANGUAGE DeriveFunctor #-}
module IOrep where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Data.Set (singleton)

import Control.Monad (ap, (>=>))

import Trace

data IOrep a
  = GetLine (Integer -> IOrep a)
  | PutChar Integer (IOrep a)
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

putChar :: Integer -> IOrep ()
putChar c = PutChar c $ pure ()

-- putStr :: [Integer] -> IOrep ()
-- putStr = mapM_ putChar

-- putStrLn :: [Integer] -> IOrep ()
-- putStrLn = putChar  . putStr
-- print :: Show a => a -> IOrep ()
--
-- getChar :: IOrep Integer
-- getChar = _

getLine :: IOrep Integer
getLine = GetLine pure

-- readLn :: Read a => a -> IOrep a


runProgram :: [Integer] -> IOrep () -> Trace
runProgram [] (GetLine _) = OutOfInputs
runProgram (i:is) (GetLine f) = ProgRead i $ runProgram is (f i)
runProgram is (PutChar n p') = ProgWrite Mandatory (singleton n) $ runProgram is p'
runProgram _ (Return ()) = Terminate
