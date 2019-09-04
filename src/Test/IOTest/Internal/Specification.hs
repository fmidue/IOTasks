{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Internal.Specification (
  Specification(..),
  Action (..),
  optional,
) where

import Prelude hiding (foldr)

import Test.IOTest.Utils
import Test.IOTest.Internal.Environment
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.ValueSet

import Data.List (nub)
import Data.Proxy
import Data.MonoTraversable
import Data.MonoTraversable.Unprefixed (foldr)

newtype Specification = Spec [Action]
  deriving (Semigroup, Monoid, MonoFoldable) via [Action]

-- for MonoFoldable
type instance Element Specification = Action

data Action where
  ReadInput :: Varname -> ValueSet -> Action
  WriteOutput :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> Action
  Branch :: Term Bool -> Specification -> Specification -> Action
  TillE :: Specification -> Action
  E :: Action

-- move into Combinators ?
optional :: Specification -> Specification
optional (Spec []) = Spec []
optional (Spec (WriteOutput p _ ps ts : xs)) = Spec [WriteOutput p True ps ts] <> optional (Spec xs)
optional _ = error "only writes can be optional"

instance HasVariables Specification where
  vars = nub . foldr phi [] where
    phi (ReadInput v _) vs = v : vs
    phi WriteOutput{} vs = vs
    phi (TillE s) vs = vars s ++ vs
    phi (Branch _ s1 s2) vs = vars s1 ++ vars s2 ++ vs
    phi E vs = vs
