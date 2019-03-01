module Test.IOTest.Type (
  Spec (..),
  andThen,
  unsugar
) where

import qualified Test.IOTest.Language as Surface
import Test.IOTest.Language (VarName,NumType,Specification((:<>)))

data Spec
  = Read VarName NumType Spec
  | Write [Surface.Function] Spec
  | Branch Surface.Predicate Spec Spec Spec
  | TillE Spec Spec
  | InternalE Spec
  | JumpPoint Spec Spec
  | Nop
  deriving Show

andThen :: Spec -> Spec -> Spec
andThen (Read xs ty s2) s' = Read xs ty (s2 `andThen` s')
andThen (Write s1 s2) s' = Write s1 $ s2 `andThen` s'
andThen (TillE s1 s2) s' = TillE s1 $ s2 `andThen` s'
andThen (Branch p s11 s12 s2) s' = Branch p s11 s12 $ s2 `andThen` s'
andThen (InternalE s) s' = InternalE $ s `andThen` s'
andThen Nop s' = s'
andThen (JumpPoint s1 s2) s' = JumpPoint s1 $ s2 `andThen` s'

unsugar :: Surface.Specification -> Spec
unsugar (Surface.ReadInput x ty :<> s') = Read x ty (unsugar s')
unsugar (Surface.WriteOutput fs :<> s') = Write fs (unsugar s')
unsugar (Surface.TillE s :<> s') = TillE (unsugar s) (unsugar s')
unsugar (Surface.Branch p s1 s2 :<> s') = Branch p (unsugar s1) (unsugar s2) (unsugar s')
unsugar ((s11 :<> s12) :<> s2) =  unsugar $ s11 :<> (s12 :<> s2)
unsugar Surface.E = InternalE Nop
unsugar Surface.Nop = Nop
unsugar s@Surface.ReadInput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.WriteOutput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.TillE{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.Branch{} = unsugar $ s :<> Surface.Nop
unsugar (Surface.E :<> _) = InternalE Nop
unsugar (Surface.Nop :<> s) = unsugar s
