module Test.IOTasks.ValueSet (
  ValueSet,
  empty, complete, singleton, fromList,
  union, intersection,
  lessThan, greaterThan,
  (\\), with, without,
  complement,
  unique, notInVar,
  isEmpty,
  containsValue, initiallyContainsValue,
  showValueSet,
  -- | = random value generation
  valueOf,
  Size(..),
  -- | = standard value sets
  ints, nats, str,
  ) where

import Test.IOTasks.Internal.ValueSet
