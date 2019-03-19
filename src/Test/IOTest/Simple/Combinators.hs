module Test.IOTest.Simple.Combinators (
  readTillFixedLength,
  readUntil,
  readUntil1,
  repeatSpec,
  when
) where

import Test.IOTest.Internal.Specification
import Test.IOTest.Internal.Term
import Control.Arrow

repeatSpec :: Int -> Specification varName Int -> Specification varName Int
repeatSpec 0 _ = Nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: Term x Int Bool -> Specification x Int -> Specification x Int
when p = Branch p Nop

readTillFixedLength :: Eq varName => varName -> NumType -> varName -> Specification varName Int
readTillFixedLength n ty xs =
  TillE $
    Branch (curry (uncurry (==) . first length) <$> getAll xs <*> getCurrent n)
      (ReadInput xs ty)
      E

readUntil :: Eq varName => varName -> ([Int] -> Bool) -> NumType -> Specification varName Int
readUntil xs p ty =
  TillE $
    Branch (p <$> getAll xs)
      (ReadInput xs ty)
      E

readUntil1 :: Eq varName => varName -> ([Int] -> Bool) -> NumType -> Specification varName Int
readUntil1 xs p ty =
  TillE $
    ReadInput xs ty <>
    Branch (p <$> getAll xs)
      Nop
      E
