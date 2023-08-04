module Test.IOTasks.ValueSet (
  ValueSet,
  empty, every, singleton, fromList,
  union, intersection,
  lessThan, greaterThan,
  (\\), with, without,
  complement,
  containsValue,
  printValueSet,
  -- | = random value generation
  valueOf,
  Size(..),
  -- | = standard value sets
  ints, nats, str,
  ) where

import Test.IOTasks.Internal.ValueSet
