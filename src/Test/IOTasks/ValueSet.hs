module Test.IOTasks.ValueSet (
  ValueSet,
  empty, complete, singleton, fromList,
  union, intersection,
  lessThan, greaterThan,
  (\\), with, without,
  complement,
  isEmpty,
  containsValue,
  showValueSet,
  -- | = random value generation
  valueOf,
  Size(..),
  -- | = standard value sets
  ints, nats, str,
  ) where

import Test.IOTasks.Internal.ValueSet
