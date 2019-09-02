{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Internal.Specification (
  Specification,
  Action (..),
  optional,
) where

import Test.IOTest.Utils
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.ValueSet

import Data.List (nub)
import Data.Proxy

type Specification = [Action]

data Action where
  ReadInput :: Varname -> ValueSet -> Action
  WriteOutput :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> Action
  Branch :: Term Bool -> Specification -> Specification -> Action
  TillE :: Specification -> Action
  E :: Action

-- move into Combinators ?
optional :: Specification -> Specification
optional [] = []
optional (WriteOutput p _ ps ts : xs) = [WriteOutput p True ps ts] <> optional xs
optional _ = error "only writes can be optional"

instance HasVariables [Action] where
  vars = nub . foldr phi [] where
    phi (ReadInput v _) vs = v : vs
    phi WriteOutput{} vs = vs
    phi (TillE s) vs = vars s ++ vs
    phi (Branch _ s1 s2) vs = vars s1 ++ vars s2 ++ vs
    phi E vs = vs
