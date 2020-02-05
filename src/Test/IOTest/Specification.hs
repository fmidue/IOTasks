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
import Test.IOTest.Term (Term, termVars)
import Test.IOTest.Pattern
import Test.IOTest.ValueSet

import Data.List (nub)
import Data.MonoTraversable
import Data.MonoTraversable.Unprefixed (foldr)

import Text.PrettyPrint.Annotated.HughesPJClass hiding ((<>))

newtype Specification = Spec [Action]
  deriving (Semigroup, Monoid, MonoFoldable) via [Action]

-- for MonoFoldable
type instance Element Specification = Action

data Action where
  ReadInput :: Varname -> ValueSet -> Action
  WriteOutput :: StringEmbedding a => Bool -> [TermPattern] -> [Term a] -> Action
  Branch :: Term Bool -> Specification -> Specification -> Action
  TillE :: Specification -> Action
  E :: Action

instance Show Specification where
  show = render . pPrint

instance Show Action where
  show = render . pPrint

instance Pretty Specification where
  pPrint (Spec as) = vcat $ pPrint <$> as

instance Pretty Action where
  pPrint (ReadInput x _) = text "ReadInput" <+> text (show x) <+> text "_"
  pPrint (WriteOutput b ps ts) = hsep [text "WriteOutput", text (show b), text (show ps), text "_{", text (show (termVars <$> ts)), text "}"]
  pPrint (Branch c s1 s2) = hang (text "Branch _{" <+> text (show $ termVars c) <+> text "}") 2 (parens (pPrint s1) $+$ parens (pPrint s2))
  pPrint (TillE s) = hang (text "TillE") 2 (parens (pPrint s))
  pPrint E = text "E"

-- move into Combinators ?
optional :: Specification -> Specification
optional (Spec []) = Spec []
optional (Spec (WriteOutput _ ps ts : xs)) = Spec [WriteOutput True ps ts] <> optional (Spec xs)
optional _ = error "only writes can be optional"

instance HasVariables Specification where
  vars = nub . foldr phi [] where
    phi (ReadInput v _) vs = v : vs
    phi (WriteOutput _ _ ts) vs = concatMap termVars ts ++ vs
    phi (TillE s) vs = vars s ++ vs
    phi (Branch c s1 s2) vs = termVars c ++ vars s1 ++ vars s2 ++ vs
    phi E vs = vs
