module Test.IOTest.Extended.Combinators
  ( readTillFixedLengthS, readTillFixedLengthI
  , readUntilS, readUntilI
  , readUntilS', readUntilI'
  , repeatSpec
  , when
) where

import Test.IOTest.Extended.Language
--import Test.IOTest.Internal.Specification
--import Test.IOTest.Internal.Term
import           Control.Arrow

repeatSpec :: Int -> Specification -> Specification
repeatSpec 0 _ = nop
repeatSpec n s = s <> repeatSpec (n-1) s

when :: Term Bool -> Specification -> Specification
when p = branch p nop

readTillFixedLengthI :: VarName -> NumType -> VarName -> Specification
readTillFixedLengthI n ty xs =
  tillE $
    branch (curry (uncurry (==) . first length) <$> getAllI xs <*> getCurrentI n)
      (readInputI xs ty)
      e

readTillFixedLengthS :: VarName -> VarName -> Specification
readTillFixedLengthS n xs =
  tillE $
    branch (curry (uncurry (==) . first length) <$> getAllS xs <*> getCurrentI n)
      (readInputS xs)
      e

readUntilS ::VarName -> ([String] -> Bool) -> Specification
readUntilS xs p =
  tillE $
    branch (p <$> getAllS xs)
      (readInputS xs)
      e

readUntilI ::VarName -> ([Int] -> Bool) -> NumType -> Specification
readUntilI xs p ty =
  tillE $
    branch (p <$> getAllI xs)
      (readInputI xs ty)
      e

readUntilS' :: VarName -> ([String] -> Bool) -> Specification
readUntilS' xs p =
  tillE $
    readInputS xs <>
    branch (p <$> getAllS xs)
      nop
      e

readUntilI' :: VarName -> ([Int] -> Bool) -> NumType -> Specification
readUntilI' xs p ty =
  tillE $
    readInputI xs ty <>
    branch (p <$> getAllI xs)
      nop
      e
