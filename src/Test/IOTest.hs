module Test.IOTest (
  -- * Specifications
  Specification(..),
  VarName,
  NumType,
  -- * Programs
  IOtt,
  IOtt.getLine,
  IOtt.putStrLn,
  IOtt.print,
  -- * Program properties
  IOProperty,
  fulfills,
  fulfillsNot,
) where

import Test.IOTest.Language
import Test.IOTest.IOtt as IOtt
import Test.IOTest.IOProperty
