module Test.IOTest (
  -- * Specifications
  Specification(..),
  VarName,
  NumType,
  Function(..),
  Predicate(..),
  -- * Programs
  IOtt,
  IOtt.getLine,
  IOtt.putStrLn,
  IOtt.print,
  -- * Program properties
  IOProperty,
  fulfills,
  fulfillsNot,
  generalize2,
  generalize3
) where

import Test.IOTest.Language
import Test.IOTest.IOtt as IOtt
import Test.IOTest.IOProperty
