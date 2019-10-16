{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Specification (
  Specification(..),
  Action (..),
  optional,
) where

import Prelude hiding (foldr)

import Test.IOTest.Utils
import Test.IOTest.Environment
import Test.IOTest.Term
import Test.IOTest.Pattern
import Test.IOTest.ValueSet

import Data.List (nub)
import Data.MonoTraversable
import Data.MonoTraversable.Unprefixed (foldr)

newtype Specification = Spec [Action]
  deriving (Semigroup, Monoid, MonoFoldable) via [Action]

-- for MonoFoldable
type instance Element Specification = Action

data Action where
  ReadInput :: Varname -> ValueSet -> Action
  WriteOutput :: StringEmbedding a => Bool -> [LinearPattern] -> [Term a] -> Action
  Branch :: Term Bool -> Specification -> Specification -> Action
  TillE :: Specification -> Action
  E :: Action

instance Show Specification where
  show (Spec as) = show as

instance Show Action where
  show (ReadInput x _) = "ReadInput " ++ show x ++ " _"
  show (WriteOutput b ps ts) = concat ["WriteOutput ", show b, " ", show ps, " _{"++ show (vars <$> ts) ++"}"]
  show (Branch c s1 s2) = concat ["Branch _{",show $ vars c,"} ", show s1, " ", show s2]
  show (TillE s) = "TillE " ++ show s
  show E = "E"

-- move into Combinators ?
optional :: Specification -> Specification
optional (Spec []) = Spec []
optional (Spec (WriteOutput _ ps ts : xs)) = Spec [WriteOutput True ps ts] <> optional (Spec xs)
optional _ = error "only writes can be optional"

instance HasVariables Specification where
  vars = nub . foldr phi [] where
    phi (ReadInput v _) vs = v : vs
    phi (WriteOutput _ _ ts) vs = concatMap vars ts ++ vs
    phi (TillE s) vs = vars s ++ vs
    phi (Branch c s1 s2) vs = vars c ++ vars s1 ++ vars s2 ++ vs
    phi E vs = vs
