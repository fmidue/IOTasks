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

ints, nats :: ValueSet
ints = Every
nats = Eq 0 `Union` GreaterThan 0
