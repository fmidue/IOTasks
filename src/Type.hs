module Type where

import qualified Language as Surface
import Language (VarName,NumType,Specification((:<>)))

data Spec
  = Read VarName NumType Spec
  | Write [Surface.Function] Spec
  | Branch Surface.Predicate Spec Spec Spec
  | TillT Spec Spec
  | InternalT Spec
  | JumpPoint Spec Spec
  | Nop
  deriving Show

andThen :: Spec -> Spec -> Spec
andThen (Read xs ty s2) s' = Read xs ty (s2 `andThen` s')
andThen (Write s1 s2) s' = Write s1 $ s2 `andThen` s'
andThen (TillT s1 s2) s' = TillT s1 $ s2 `andThen` s'
andThen (Branch p s11 s12 s2) s' = Branch p s11 s12 $ s2 `andThen` s'
andThen (InternalT s) s' = InternalT $ s `andThen` s'
andThen Nop s' = s'
andThen (JumpPoint s1 s2) s' = JumpPoint s1 $ s2 `andThen` s'

unsugar :: Surface.Specification -> Spec
unsugar (Surface.ReadInput x ty :<> s') = Read x ty (unsugar s')
unsugar (Surface.WriteOutput fs :<> s') = Write fs (unsugar s')
unsugar (Surface.TillT s :<> s') = TillT (unsugar s) (unsugar s')
unsugar (Surface.Branch p s1 s2 :<> s') = Branch p (unsugar s1) (unsugar s2) (unsugar s')
unsugar ((s11 :<> s12) :<> s2) =  unsugar $ s11 :<> (s12 :<> s2)
unsugar Surface.T = InternalT Nop
unsugar Surface.Nop = Nop
unsugar s@Surface.ReadInput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.WriteOutput{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.TillT{} = unsugar $ s :<> Surface.Nop
unsugar s@Surface.Branch{} = unsugar $ s :<> Surface.Nop
unsugar (Surface.T :<> _) = InternalT Nop
unsugar (Surface.Nop :<> s) = unsugar s
