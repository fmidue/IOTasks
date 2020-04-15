{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Combinators (
  readTillFixedLength,
  readUntil,
  repeatSpec,
  when,
) where

import Test.IOTasks.Language
import Test.IOTasks.Term (SpecVar)

import Data.Dynamic (Typeable)

repeatSpec :: Int -> Specification t -> Specification t
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n Prelude.- 1) s

when :: t SpecVar Bool -> Specification t -> Specification t
when p = branch p nop

readTillFixedLength :: forall a t. (Term t, Typeable a) => (t SpecVar [a] -> t SpecVar Int) -> (t SpecVar Int -> t SpecVar Int -> t SpecVar Bool) -> Varname -> ValueSet -> Varname -> Specification t
readTillFixedLength len eq n vs xs =
  tillExit $
    branch (len (getAll @a xs) `eq` getCurrent n)
      (readInput xs vs)
      exit

readUntil :: Varname -> t SpecVar Bool -> ValueSet -> Specification t
readUntil xs p vs =
  tillExit $
    branch p
      (readInput xs vs)
      exit
