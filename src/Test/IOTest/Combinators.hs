{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Combinators (
  readTillFixedLength,
  readUntil,
  readUntil1,
  repeatSpec,
  when
) where

import Test.IOTest.Language
import Test.IOTest.Internal.ValueSet

import Data.Proxy
import System.Random

repeatSpec :: Int -> Specification -> Specification
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: Term Bool -> Specification -> Specification
when p = branch p nop

readTillFixedLength :: Varname -> ValueSet -> Varname -> Specification
readTillFixedLength n vs xs = elimValueSet vs (undefined :: StdGen) $ \(p :: Proxy s) _ (_ :: b) ->
  tillE $
    branch ((\xs n -> length xs == n) <$> getAllGeneric @s @b p xs <*> getCurrent n)
      (readInput xs vs)
      e

readUntil :: Varname -> Term Bool -> ValueSet -> Specification
readUntil xs p vs =
  tillE $
    branch p
      (readInput xs vs)
      e

readUntil1 :: Varname -> Term Bool -> ValueSet -> Specification
readUntil1 xs p ty =
  tillE $
    readInput xs ty <>
    branch p
      nop
      e
