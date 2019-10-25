{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Combinators (
  readTillFixedLength,
  readUntil,
  repeatSpec,
  when
) where

import Test.IOTest.Language
import Test.IOTest.ValueSet

import Data.Proxy

repeatSpec :: Int -> Specification -> Specification
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: Term Bool -> Specification -> Specification
when p = branch p nop

readTillFixedLength :: Varname -> ValueSet -> Varname -> Specification
readTillFixedLength n vs xs = withProxy vs $ \(_ :: Proxy a) ->
  tillExit $
    branch ((\xs n -> length xs == n) <$> getAllGeneric @a xs <*> getCurrent n)
      (readInput xs vs)
      exit

readUntil :: Varname -> Term Bool -> ValueSet -> Specification
readUntil xs p vs =
  tillExit $
    branch p
      (readInput xs vs)
      exit

readUntil1 :: Varname -> Term Bool -> ValueSet -> Specification
readUntil1 xs p ty =
  tillExit $
    readInput xs ty <>
    branch p
      nop
      exit
