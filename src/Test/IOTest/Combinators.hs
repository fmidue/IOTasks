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

repeatSpec :: Int -> Specification t -> Specification t
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: t Bool -> Specification t -> Specification t
when p = branch p nop

readTillFixedLength :: (Term t, Applicative t) => Varname -> ValueSet -> Varname -> Specification t
readTillFixedLength n vs xs = withProxy vs $ \(_ :: Proxy a) ->
  tillExit $
    branch ((\xs n -> length xs == n) <$> getAll @a xs <*> getCurrent n)
      (readInput xs vs)
      exit

readUntil :: Varname -> t Bool -> ValueSet -> Specification t
readUntil xs p vs =
  tillExit $
    branch p
      (readInput xs vs)
      exit
