{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Internal.Specification (
  Specification(..),
  SpecificationF(..),
  optional,
) where

import Test.IOTest.Utils
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.ValueSet

import Data.List (nub)
import Data.Proxy
import Data.Functor.Foldable

data Specification where
  ReadInput ::Varname -> ValueSet -> Specification
  WriteOutput :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> Specification
  Branch :: Term Bool -> Specification -> Specification -> Specification
  TillE :: Specification -> Specification
  Nop :: Specification
  E :: Specification
  (:<>) :: Specification -> Specification -> Specification

infixr 6 :<>

data SpecificationF f where
  ReadInputF :: Varname -> ValueSet -> SpecificationF f
  WriteOutputF :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> SpecificationF f
  BranchF :: Term Bool -> f -> f -> SpecificationF f
  TillEF :: f -> SpecificationF f
  NopF :: SpecificationF f
  EF :: SpecificationF f
  (:<>$) :: f -> f -> SpecificationF f

deriving instance Functor SpecificationF
deriving instance Foldable SpecificationF
deriving instance Traversable SpecificationF

type instance Base Specification = SpecificationF

instance Recursive Specification where
  project (ReadInput x r) = ReadInputF x r
  project (WriteOutput p opt ps ts) = WriteOutputF p opt ps ts
  project (Branch p s1 s2) = BranchF p s1 s2
  project (TillE s) = TillEF s
  project Nop = NopF
  project E = EF
  project (s1 :<> s2) = s1 :<>$ s2

instance Corecursive Specification where
  embed (ReadInputF x r) = ReadInput x r
  embed (WriteOutputF p opt ps ts) = WriteOutput p opt ps ts
  embed (BranchF p s1 s2) = Branch p s1 s2
  embed (TillEF s) = TillE s
  embed NopF = Nop
  embed EF = E
  embed (s1 :<>$ s2) = s1 :<> s2

-- move into Combinators ?
optional :: Specification -> Specification
optional (WriteOutput p _ ps ts) = WriteOutput p True ps ts
optional _ = error "only writes can be optional"

instance Semigroup Specification where
  (<>) = (:<>)

instance HasVariables Specification where
  vars = nub . cata phi where
    phi (ReadInputF v _) = [v]
    phi (s1 :<>$ s2) = s1 ++ s2
    phi (TillEF s) = s
    phi (BranchF _ s1 s2) = s1 ++ s2
    phi WriteOutputF{} = []
    -- phi (WriteOutputPF _) = []
    phi NopF = []
    phi EF = []
