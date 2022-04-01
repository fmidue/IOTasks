module Example where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn
  ,until)


import Specification
import IOrep
import Term
import ValueSet

--example specifications

example :: Specification
example =
  readInput "x" ints <>
  readInput "y" nats <>
  branch (Current "x" :>: Current "y")
    (writeOutput (Current "x" :+: Current "y") )
    (writeOutput (Current "x" :*: Current "y") )


example2 :: Specification
example2 =
  readInput "n" nats <>
  until (Length (All "x") :==: Current "n")
    (readInput "x" ints ) <>
  writeOutput (Sum $ All "x")


example3 :: Specification
example3 =
  readInput "n" nats <>
  until (Sum (All "x") :>: Current "n")
    (readInput "x" ints) <>
  writeOutput (Length $ All "x")


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

prog3 :: IOrep ()
prog3 = do
  n <- getLine
  let
    loop s m
      | s > n  = putChar m
      | otherwise = do
        x <- getLine
        loop (s+x) (m+1)
  loop 0 0
