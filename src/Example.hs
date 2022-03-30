module Example where

import Specification
import Term
import ValueSet

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
  WriteOutput (Current "x")
  Nop

ints, nats :: ValueSet
ints = Every
nats = Eq 0 `Union` GreaterThan 0
