{-# LANGUAGE GADTs #-}
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

data Spec a
  = Read VarName (Restriction a) (Spec a)
  | Write [Term VarName a a] (Spec a)
  | WriteP [AbstractPattern VarName a a] (Spec a)
  | Branch (Term VarName a Bool) (Spec a) (Spec a) (Spec a)
  | TillE (Spec a) (Spec a)
  | InternalE (Spec a)
  | JumpPoint (Spec a) (Spec a)
  | Nop

andThen :: Spec a -> Spec a -> Spec a
andThen (Read xs ty s2) s' = Read xs ty (s2 `andThen` s')
andThen (Write s1 s2) s' = Write s1 $ s2 `andThen` s'
andThen (WriteP p s2) s' = WriteP p $ s2 `andThen` s'
andThen (TillE s1 s2) s' = TillE s1 $ s2 `andThen` s'
andThen (Branch p s11 s12 s2) s' = Branch p s11 s12 $ s2 `andThen` s'
andThen (InternalE s) s' = InternalE $ s `andThen` s'
andThen Nop s' = s'
andThen (JumpPoint s1 s2) s' = JumpPoint s1 $ s2 `andThen` s'

unsugar :: Surface.Specification VarName a -> Spec a
unsugar (Surface.ReadInput x ty :<> s') = Read x ty (unsugar s')
unsugar (Surface.WriteOutput fs :<> s') = Write fs (unsugar s')
unsugar (Surface.WriteOutputP p :<> s') = WriteP p (unsugar s')
unsugar (Surface.TillE s :<> s') = TillE (unsugar s) (unsugar s')
unsugar (Surface.Branch p s1 s2 :<> s') = Branch p (unsugar s1) (unsugar s2) (unsugar s')
unsugar ((s11 :<> s12) :<> s2) =  unsugar $ s11 :<> (s12 :<> s2)
unsugar Surface.E = InternalE Nop
unsugar Surface.Nop = Nop
unsugar s@Surface.ReadInput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.WriteOutput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.WriteOutputP{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.TillE{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.Branch{} = unsugar $ s :<> Surface.Nop
unsugar (Surface.E :<> _) = InternalE Nop
unsugar (Surface.Nop :<> s) = unsugar s

instance HasVariables (Spec a) where
  vars = nub . go where
    go (Read x _ s) = x : vars s
    go (TillE s s') = vars s ++ vars s'
    go (Branch _ s1 s2 s3) = vars s1 ++ vars s2 ++ vars s3
    go (Write _ s) = vars s
    go (WriteP _ s) = vars s
    go Nop = []
    go (InternalE s) = vars s
    go (JumpPoint s s') = vars s ++ vars s'
