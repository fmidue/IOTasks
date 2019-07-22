{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IOTest.Internal.InternalSpec (
  Spec (..),
  andThen,
  unsugar
) where

import           Test.IOTest.Utils
import           Test.IOTest.Internal.Context
import           Test.IOTest.Internal.Term
import           Test.IOTest.Internal.Pattern
import           Test.IOTest.Internal.ValueSet
import qualified Test.IOTest.Internal.Specification as Surface
import           Test.IOTest.Internal.Specification (Specification((:<>)))

import           Data.List (nub)
import           Data.Proxy

import Data.Functor.Foldable

data Spec where
  Read :: Varname -> ValueSet -> Spec -> Spec
  Write :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> Spec -> Spec
  Branch :: Term Bool -> Spec -> Spec -> Spec -> Spec
  TillE :: Spec -> Spec -> Spec
  InternalE :: Spec -> Spec
  Nop :: Spec

data SpecF f where
  ReadF :: Varname -> ValueSet -> f -> SpecF f
  WriteF :: StringEmbedding s a => Proxy s -> Bool -> [LinearPattern] -> [Term a] -> f -> SpecF f
  BranchF :: Term Bool -> f -> f -> f -> SpecF f
  TillEF :: f -> f -> SpecF f
  InternalEF :: f -> SpecF f
  NopF :: SpecF f

deriving instance Functor SpecF
deriving instance Foldable SpecF
deriving instance Traversable SpecF

type instance Base Spec = SpecF

instance Recursive Spec where
  project (Read x vs s) = ReadF x vs s
  project (Write pxy opt ps ts s) = WriteF pxy opt ps ts s
  project (Branch p s11 s12 s2) = BranchF p s11 s12 s2
  project (TillE s s') = TillEF s s'
  project (InternalE s) = InternalEF s
  project Nop = NopF

instance Corecursive Spec where
  embed (ReadF x vs s) = Read x vs s
  embed (WriteF pxy opt ps ts s) = Write pxy opt ps ts s
  embed (BranchF p s11 s12 s2) = Branch p s11 s12 s2
  embed (TillEF s s') = TillE s s'
  embed (InternalEF s) = InternalE s
  embed NopF = Nop

andThen :: Spec -> Spec -> Spec
andThen s1 s2 = para phi s1 where
  phi (ReadF x r (_,s)) = Read x r s
  phi (WriteF pxy opt ps ts (_,s)) = Write pxy opt ps ts s
  -- phi (WritePF ts (_,s)) = WriteP ts s
  phi (BranchF p (s11,_) (s12,_) (_,s)) = Branch p s11 s12 s
  phi (TillEF (s,_) (_,s')) = TillE s s'
  phi (InternalEF (_,s)) = InternalE s
  phi NopF = s2

unsugar :: Surface.Specification -> Spec
unsugar = ana psi where
  psi (Surface.ReadInput x ty :<> s') = ReadF x ty s'
  psi (Surface.WriteOutput pxy opt ps ts :<> s') = WriteF pxy opt ps ts s'
  -- psi (Surface.WriteOutputP p :<> s') = WritePF p s'
  psi (Surface.TillE s :<> s') = TillEF s s'
  psi (Surface.Branch p s1 s2 :<> s') = BranchF p s1 s2 s'
  psi Surface.E = InternalEF nop
  psi Surface.Nop = NopF
  -- --normalizing rewrites
  psi ((s11 :<> s12) :<> s2) = psi $ s11 :<> (s12 :<> s2)
  psi (Surface.Nop :<> s) = psi s
  psi (Surface.ReadInput x ty) = ReadF x ty nop
  psi (Surface.WriteOutput pxy opt ps ts) = WriteF pxy opt ps ts nop
  -- psi (Surface.WriteOutputP ts) = WritePF ts nop
  psi (Surface.TillE s) = TillEF s nop
  psi (Surface.Branch p s1 s2) = BranchF p s1 s2 nop
  psi (Surface.E :<> _) = InternalEF nop
  -- helper
  nop = Surface.Nop

instance HasVariables Spec where
  vars = nub . cata phi where
    phi (TillEF s s') = s ++ s'
    phi (ReadF x _ s) = x : s
    phi (BranchF _ s1 s2 s3) = s1 ++ s2 ++ s3
    phi (WriteF _ _ _ _ s) = s
    -- phi (WritePF _ s) = s
    phi NopF = []
    phi (InternalEF s) = s
