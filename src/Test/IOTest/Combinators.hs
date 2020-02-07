{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Combinators (
  readTillFixedLength,
  readUntil,
  repeatSpec,
  when
) where

import Test.IOTest
import Test.IOTest.ValueSet

import Data.Dynamic (Typeable)

repeatSpec :: Int -> Specification t -> Specification t
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n Prelude.- 1) s

when :: t Bool -> Specification t -> Specification t
when p = branch p nop

readTillFixedLength :: forall t a. (Term t, Typeable a) => (t [a] -> t Int) -> (t Int -> t Int -> t Bool) -> Varname -> ValueSet -> Varname -> Specification t
readTillFixedLength len eq n vs xs =
  tillExit $
    branch (len (getAll @a xs) `eq` getCurrent n)
      (readInput xs vs)
      exit

readUntil :: Varname -> t Bool -> ValueSet -> Specification t
readUntil xs p vs =
  tillExit $
    branch p
      (readInput xs vs)
      exit
