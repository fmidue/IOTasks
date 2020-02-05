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

import Test.IOTest.Term as T

import Data.Proxy

repeatSpec :: Int -> Specification -> Specification
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n Prelude.- 1) s

when :: Term Bool -> Specification -> Specification
when p = branch p nop

readTillFixedLength :: Varname -> ValueSet -> Varname -> Specification
readTillFixedLength n vs xs = withProxy vs $ \(_ :: Proxy a) ->
  tillExit $
    branch (T.length (T.getAll @a xs) T.== T.getCurrent n)
      (readInput xs vs)
      exit

readUntil :: Varname -> Term Bool -> ValueSet -> Specification
readUntil xs p vs =
  tillExit $
    branch p
      (readInput xs vs)
      exit
