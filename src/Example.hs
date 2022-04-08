{-# LANGUAGE TypeApplications #-}
module Example where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn
  ,until)


import Specification
import IOrep
import Term
import ValueSet
import OutputPattern

--example specifications

example1 :: Specification
example1 =
  readInput "x" ints AssumeValid <>
  readInput "y" nats AssumeValid <>
  branch (Current "x" :>: Current "y")
    (writeOutput [Wildcard <> Value (Current "x" :+: Current "y") <> Wildcard , Value (Current "x" :-: Current "y") <> Wildcard] )
    (writeOutput [Wildcard <> Text "Result: " <> Value (Current "x" :*: Current "y")] )

example2 :: Specification
example2 =
  readInput "n" nats UntilValid <>
  until (Length (All "x") :==: Current "n")
    (writeOptionalOutput [Value $ Current "n" :-: Length (All "x")] <> readInput "x" ints AssumeValid) <>
  writeOutput [Value $ Sum $ All "x"]

example3 :: Specification
example3 =
  readInput "n" nats AssumeValid <>
  until (Sum (All "x") :>: Current "n")
    (readInput "x" ints AssumeValid) <>
  writeOutput [Value $ Length $ All "x"]

-- atempt at 'breaking' the solver
example4 :: Specification
example4 =
  readInput "x" nats AssumeValid <>
  until (Current "x" :==: Product (All "y"))
    (readInput "y" nats AssumeValid)

ints, nats :: ValueSet
ints = Every
nats = Eq 0 `Union` GreaterThan 0

-- example programs

prog1 :: IOrep ()
prog1 = do
  x <- readLn @Integer
  y <- readLn
  if x > y
    then print $ x + y
    else putStr "Result: " >> print (x * y)

prog2 :: IOrep ()
prog2 = do
  n <- readLn @Integer
  if n < 0
    then prog2
    else
      let
        loop 0 x = print @Integer x
        loop m x = do
          -- putChar m
          i <- readLn
          loop (m-1) (x+i)
      in loop n 0

prog3 :: IOrep ()
prog3 = do
  n <- readLn @Integer
  let
    loop s m
      | s > n  = print @Integer m
      | otherwise = do
        x <- readLn
        loop (s+x) (m+1)
  loop 0 0

prog4 :: IOrep ()
prog4 = do
  x <- readLn @Integer
  let
    loop p
      | p == x = pure ()
      | otherwise = do
        y <- readLn
        loop (p*y)
  loop 1
