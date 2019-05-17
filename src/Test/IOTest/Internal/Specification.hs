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
  VarName,
  Opt(..),
  NumType(..),
  matchesType,
  optional,
  Restrictable(..),
  AbstractPattern(Exactly,Contains,Everything)
) where

import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Pattern

import Test.QuickCheck hiding (Positive)

import Data.List (nub)
import Data.Functor.Foldable

type VarName = String

data NumType = IntTy | NatTy | Positive | Negative | Zero | Not NumType deriving (Eq,Ord,Read,Show)

matchesType :: NumType -> Int -> Bool
matchesType IntTy = const True
matchesType NatTy = (>= 0)
matchesType Positive = (> 0)
matchesType Negative = (< 0)
matchesType Zero = (== 0)
matchesType (Not ty) = not . matchesType ty

data Specification v a where
  ReadInput :: v -> Restriction a -> Specification v a
  WriteOutput :: [Term v a a] -> Specification v a
  WriteOutputP :: [AbstractPattern v String String] -> Specification v String
  Branch :: Term v a Bool -> Specification v a -> Specification v a -> Specification v a
  TillE :: Specification v a -> Specification v a
  Nop :: Specification v a
  E :: Specification v a
  (:<>) :: Specification v a -> Specification v a -> Specification v a

infixr 6 :<>

data SpecificationF v a f where
  ReadInputF :: v -> Restriction a -> SpecificationF v a f
  WriteOutputF :: [Term v a a] -> SpecificationF v a f
  WriteOutputPF :: [AbstractPattern v String String] -> SpecificationF v String f
  BranchF :: Term v a Bool -> f -> f -> SpecificationF v a f
  TillEF :: f -> SpecificationF v a f
  NopF :: SpecificationF v a f
  EF :: SpecificationF v a f
  (:<>$) :: f -> f -> SpecificationF v a f

deriving instance Functor (SpecificationF v a)
deriving instance Foldable (SpecificationF v a)
deriving instance Traversable (SpecificationF v a)

type instance Base (Specification v a) = SpecificationF v a

instance Recursive (Specification v a) where
  project (ReadInput x r) = ReadInputF x r
  project (WriteOutput ts) = WriteOutputF ts
  project (WriteOutputP ts) = WriteOutputPF ts
  project (Branch p s1 s2) = BranchF p s1 s2
  project (TillE s) = TillEF s
  project Nop = NopF
  project E = EF
  project (s1 :<> s2) = s1 :<>$ s2

instance Corecursive (Specification v a) where
  embed (ReadInputF x r) = ReadInput x r
  embed (WriteOutputF ts) = WriteOutput ts
  embed (WriteOutputPF ts) = WriteOutputP ts
  embed (BranchF p s1 s2) = Branch p s1 s2
  embed (TillEF s) = TillE s
  embed NopF = Nop
  embed EF = E
  embed (s1 :<>$ s2) = s1 :<> s2

-- move into Combinators ?
optional :: Specification v a -> Specification v a
optional (WriteOutput fs) = WriteOutput (epsilon : fs)
optional (WriteOutputP ps) = WriteOutputP (NoOutput : ps)
optional _ = error "only writes can be optional"

instance Semigroup (Specification v a) where
  (<>) = (:<>)

data Opt = Can | Must

instance HasVariables (Specification VarName a) where
  vars = nub . cata phi where
    phi (ReadInputF v _) = [v]
    phi (s1 :<>$ s2) = s1 ++ s2
    phi (TillEF s) = s
    phi (BranchF _ s1 s2) = s1 ++ s2
    phi (WriteOutputF _) = []
    phi (WriteOutputPF _) = []
    phi NopF = []
    phi EF = []

instance Show (Specification v a) where
  show _ = "HACK"

class Restrictable a where
  type Restriction a :: *
  restrict :: Restriction a -> Gen a

instance Restrictable Int where
  type Restriction Int = NumType
  restrict = intGen

instance Restrictable String where
  type Restriction String = Maybe NumType
  restrict (Just ty) = show <$> intGen ty
  restrict Nothing = arbitrary

intGen :: NumType -> Gen Int
intGen IntTy = choose (-10,10)
intGen NatTy = choose (0,10)
intGen Positive = choose (1,10)
intGen Negative = choose (-10,-1)
intGen Zero = return 0
intGen (Not IntTy) = error "empty type"
intGen (Not NatTy) = intGen Negative
intGen (Not Positive) = choose (-10,0)
intGen (Not Negative) = intGen NatTy
intGen (Not Zero) = oneof [intGen Negative, intGen Positive]
intGen (Not (Not ty)) = intGen ty
