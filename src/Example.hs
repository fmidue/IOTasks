module Example where
import Prelude hiding (putChar,putStr,putStrLn,print,getChar,getLine,readLn)


import Specification
import IOrep
import Term
import ValueSet

--example specifications

example :: Specification
example =
  ReadInput "x" ints $
  ReadInput "y" nats $
  Branch (Current "x" :>: Current "y")
    (WriteOutput (Current "x" :+: Current "y") Nop)
    (WriteOutput (Current "x" :*: Current "y") Nop)
  Nop

example2 :: Specification
example2 =
  ReadInput "n" nats $
  Until (Length (All "x") :==: Current "n")
    (ReadInput "x" ints Nop) $
  WriteOutput (Sum $ All "x")
  Nop

ints, nats :: ValueSet
ints = Every
nats = Eq 0 `Union` GreaterThan 0

-- example programs

prog1 :: IOrep ()
prog1 = do
  x <- getLine
  y <- getLine
  if x > y
    then putChar $ x + y
    else putChar $ x * y

prog2 :: IOrep ()
prog2 = do
  n <- getLine
  let
    loop 0 x = putChar x
    loop m x = do
      i <- getLine
      loop (m-1) (x+i)
  loop n 0
