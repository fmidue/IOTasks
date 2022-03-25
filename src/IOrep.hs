{-# LANGUAGE DeriveFunctor #-}
module IOrep where
import Control.Monad (ap, (>=>))

import Trace

data IOrep a
  = GetLine (Int -> IOrep a)
  | PutChar Int (IOrep a)
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

runProgram :: [Int] -> IOrep () -> Trace
runProgram [] (GetLine _) = error "out of inputs"
runProgram (i:is) (GetLine f) = ProgRead i $ runProgram is (f i)
runProgram is (PutChar n p') = ProgWrite n $ runProgram is p'
runProgram _ (Return ()) = Terminate
