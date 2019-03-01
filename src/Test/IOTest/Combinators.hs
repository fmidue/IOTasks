module Test.IOTest.Combinators (
  readTillFixedLength,
  readUntil,
  readUntil1,
  repeatSpec,
  when
) where

import Test.IOTest.Language

repeatSpec :: Int -> Specification varName -> Specification varName
repeatSpec 0 _ = Nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: Predicate x -> Specification x -> Specification x
when p = Branch p Nop

readTillFixedLength :: varName -> NumType -> varName -> Specification varName
readTillFixedLength n ty xs =
  TillE $
    Branch (MixedP (\ys m -> length ys == m) (xs,n))
      (ReadInput xs ty)
      E

readUntil :: varName -> ([Int] -> Bool) -> NumType -> Specification varName
readUntil xs p ty =
  TillE $
    Branch (UListP p xs)
      (ReadInput xs ty)
      E

readUntil1 :: varName -> ([Int] -> Bool) -> NumType -> Specification varName
readUntil1 xs p ty =
  TillE $
    ReadInput xs ty <>
    Branch (UListP p xs)
      Nop
      E
