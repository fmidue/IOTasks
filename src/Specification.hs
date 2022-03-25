{-# LANGUAGE GADTs #-}
module Specification where

import ValueSet
import Term

data Specification where
  ReadInput :: Varname -> ValueSet -> Specification -> Specification
  WriteOutput :: Term Int -> Specification -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification -> Specification
  Nop :: Specification

instance Semigroup Specification where
  s <> Nop = s
  Nop <> s = s
  (ReadInput x vs s) <> s' = ReadInput x vs $ s <> s'
  (WriteOutput t s) <> s' = WriteOutput t $ s <> s'
  (Branch c l r s) <> s' = Branch c l r $ s <> s'

instance Monoid Specification where
  mempty = Nop
