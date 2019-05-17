{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.IOTest.Internal.InternalSpec (
  Spec (..),
  andThen,
  unsugar
) where

import           Test.IOTest.Internal.Context
import           Test.IOTest.Internal.Term
import           Test.IOTest.Internal.Pattern
import qualified Test.IOTest.Internal.Specification as Surface
import           Test.IOTest.Internal.Specification (Restriction,VarName,Specification((:<>)))

import           Data.List (nub)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

data Spec a
  = Read VarName (Restriction a) (Spec a)
  | Write [Term VarName a a] (Spec a)
  | WriteP [AbstractPattern VarName a a] (Spec a)
  | Branch (Term VarName a Bool) (Spec a) (Spec a) (Spec a)
  | TillE (Spec a) (Spec a)
  | InternalE (Spec a)
  | JumpPoint (Spec a) (Spec a)
  | Nop

makeBaseFunctor ''Spec

andThen :: Spec a -> Spec a -> Spec a
andThen s1 s2 = para phi s1 where
  phi (ReadF x r (_,s)) = Read x r s
  phi (WriteF ts (_,s)) = Write ts s
  phi (WritePF ts (_,s)) = WriteP ts s
  phi (BranchF p (s11,_) (s12,_) (_,s)) = Branch p s11 s12 s
  phi (TillEF (s,_) (_,s')) = TillE s s'
  phi (InternalEF (_,s)) = InternalE s
  phi (JumpPointF (s,_) (_,s')) = JumpPoint s s'
  phi NopF = s2

unsugar :: Surface.Specification VarName a -> Spec a
unsugar = ana psi where
  psi (Surface.ReadInput x ty :<> s') = ReadF x ty s'
  psi (Surface.WriteOutput fs :<> s') = WriteF fs s'
  psi (Surface.WriteOutputP p :<> s') = WritePF p s'
  psi (Surface.TillE s :<> s') = TillEF s s'
  psi (Surface.Branch p s1 s2 :<> s') = BranchF p s1 s2 s'
  psi Surface.E = InternalEF nop
  psi Surface.Nop = NopF
  -- --normalizing rewrites
  psi ((s11 :<> s12) :<> s2) = psi $ s11 :<> (s12 :<> s2)
  psi (Surface.Nop :<> s) = psi s
  psi (Surface.ReadInput x ty) = ReadF x ty nop
  psi (Surface.WriteOutput ts) = WriteF ts nop
  psi (Surface.WriteOutputP ts) = WritePF ts nop
  psi (Surface.TillE s) = TillEF s nop
  psi (Surface.Branch p s1 s2) = BranchF p s1 s2 nop
  psi (Surface.E :<> _) = InternalEF nop
  -- helper
  nop = Surface.Nop

instance HasVariables (Spec a) where
  vars = nub . cata phi where
    phi (TillEF s s') = s ++ s'
    phi (ReadF x _ s) = x : s
    phi (BranchF _ s1 s2 s3) = s1 ++ s2 ++ s3
    phi (WriteF _ s) = s
    phi (WritePF _ s) = s
    phi NopF = []
    phi (InternalEF s) = s
    phi (JumpPointF s s') = s ++ s'
