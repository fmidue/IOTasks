{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Specification (
  Specification(..),
  Action (..),
  optional,
  specVars,
) where

import Prelude hiding (foldr)

import Test.IOTasks.Utils
import Data.Environment
import Test.IOTasks.Pattern
import Test.IOTasks.ValueSet
import Data.Term.AST (AST, printAST)
import Data.Term

import Data.List (nub)
import Data.MonoTraversable
import Data.MonoTraversable.Unprefixed (foldr)

import Text.PrettyPrint.Annotated.HughesPJClass (Pretty)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as PP

newtype Specification t = Spec [Action (Specification t) t]
  deriving (Semigroup, Monoid, MonoFoldable) via [Action (Specification t) t]

-- for MonoFoldable
type instance Element (Specification t) = Action (Specification t) t

data Action r t where
  ReadInput :: Varname -> ValueSet -> Action r t
  WriteOutput :: StringEmbedding a => Bool -> [TermPattern] -> [t a] -> Action r t
  Branch :: t Bool -> r -> r -> Action r t
  TillE :: r -> Action r t
  E :: Action r t

instance SynTerm t (AST Varname) => Show (Specification t) where
  show = PP.render . PP.pPrint

instance (Pretty r, SynTerm t (AST Varname)) => Show (Action r t) where
  show = PP.render . PP.pPrint

instance SynTerm t (AST Varname) => Pretty (Specification t) where
  pPrint (Spec []) = PP.text "0" PP.$$ PP.text " "
  pPrint (Spec as) = PP.vcat (PP.pPrint <$> as) PP.$$ PP.text " "

instance (Pretty r, SynTerm t (AST Varname)) => Pretty (Action r t) where
  pPrint (ReadInput x _) = PP.text "ReadInput" PP.<+> PP.text (show x) PP.<+> PP.text "_"
  pPrint (WriteOutput b ps ts) = PP.hsep [PP.text "WriteOutput", PP.text (show b), PP.text (show ps), PP.text (show (printAST . viewTerm @_ @(AST Varname) <$> ts))]
  pPrint (Branch c s1 s2) = PP.hang (PP.text "Branch" PP.<+> PP.parens (PP.text $ printAST $ viewTerm @_ @(AST Varname) c)) 2 (PP.parens (PP.pPrint s1) PP.$+$ PP.parens (PP.pPrint s2))
  pPrint (TillE s) = PP.hang (PP.text "TillE") 2 (PP.parens (PP.pPrint s))
  pPrint E = PP.text "E"

-- move into Combinators ?
optional :: Specification t -> Specification t
optional (Spec []) = Spec []
optional (Spec (WriteOutput _ ps ts : xs)) = Spec [WriteOutput True ps ts] <> optional (Spec xs)
optional _ = error "only writes can be optional"

specVars :: VarListTerm t Varname => Specification t -> [Varname]
specVars = nub . foldr phi [] where
  phi :: VarListTerm t Varname => Action (Specification t) t -> [Varname] -> [Varname]
  phi (ReadInput v _) vs = v : vs
  phi (WriteOutput _ _ ts) vs = concatMap termVars ts ++ vs
  phi (TillE s) vs = specVars s ++ vs
  phi (Branch c s1 s2) vs = termVars c ++ specVars s1 ++ specVars s2 ++ vs
  phi E vs = vs
