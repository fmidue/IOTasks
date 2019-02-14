{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Type where

import Bound

import Control.Monad (ap)

import Data.Functor.Classes
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

data Position = First | Second deriving (Eq,Ord,Read,Show)

type Spec = Everything
type Term = Everything
type Predicate = Everything
type GlobalV = Everything

type VarName = String

data NumType = IntTy | NatTy | Positive | Negative | Zero | Not NumType deriving (Eq,Ord,Read,Show)
matchesType :: NumType -> Int -> Bool
matchesType IntTy = const True
matchesType NatTy = (>= 0)
matchesType Positive = (> 0)
matchesType Negative = (< 0)
matchesType Zero = (== 0)
matchesType (Not ty) = not . matchesType ty

data Everything a
  -- Variables
  = V a
  -- Spec
  -- first parameter is the name of the global accumulator
  | Read VarName NumType (Scope () Everything a)
  | Write [Term a] (Spec a)
  | TillT (Spec a) (Spec a)
  | Branch (Predicate a) (Spec a) (Spec a) (Spec a)
  | T
  | InternalT (Spec a)
  | Nop
  | JumpPoint (Spec a) (Spec a)
  -- Term
  | Lit Int
  | Nil
  | Add (Term a) (Term a)
  | Sub (Term a) (Term a)
  | Mul (Term a) (Term a)
  | Cons (Term a) (Term a)
  | Len (Term a)
  | Sum (Term a)
  -- Predicates
  | TT
  | FF
  | And (Predicate a) (Predicate a)
  | Or (Predicate a) (Predicate a)
  | Neg (Predicate a)
  | Eq (Term a) (Term a)
  | Less (Term a) (Term a)
  deriving (Functor, Foldable, Traversable)

readInput :: VarName -> NumType -> VarName -> Spec VarName -> Spec VarName
readInput x ty xs s' = Read xs ty $ abstract1 x s'

writeOutput :: [Term a] -> Spec a -> Spec a
writeOutput = Write

tillT :: Spec a -> Spec a -> Spec a
tillT = TillT

andThen :: Spec a -> Spec a -> Spec a
andThen (Read xs ty s2) s' =
  let s2' = toScope (fromScope s2 `andThen` (F <$> s'))
  in Read xs ty s2'
andThen (Write s1 s2) s' = Write s1 $ s2 `andThen` s'
andThen (TillT s1 s2) s' = TillT s1 $ s2 `andThen` s'
andThen (Branch p s11 s12 s2) s' = Branch p s11 s12 $ s2 `andThen` s'
andThen T _ = T
andThen (InternalT s) s' = InternalT $ s `andThen` s'
andThen Nop s' = s'
andThen (JumpPoint s1 s2) s' = JumpPoint s1 $ s2 `andThen` s'
andThen _ _ = error "Not a spec"

unsugarT :: Spec a -> Spec a
unsugarT T = InternalT Nop
unsugarT (Read xs ty s2) =
  let s2' = toScope $ unsugarT $ fromScope s2
  in Read xs ty s2'
unsugarT (Write t s2) = Write t $ unsugarT s2
unsugarT (TillT s s') = TillT (unsugarT s) (unsugarT s')
unsugarT (Branch p s1 s2 s3) = Branch p (unsugarT s1) (unsugarT s2) (unsugarT s3)
unsugarT (InternalT s) = InternalT $ unsugarT s
unsugarT Nop = Nop
unsugarT (JumpPoint s1 s2) = JumpPoint (unsugarT s1) (unsugarT s2)
unsugarT x = x

instance Applicative Everything where pure = V; (<*>) = ap
instance Monad Everything where
  return = V
  -- Variables
  V a >>= f = f a
  -- Spec
  T >>= _ = T
  InternalT s >>= f = InternalT (s >>= f)
  Nop >>= _ = Nop
  Write t s >>= f = Write ((>>= f) <$> t) (s >>= f)
  Read acc ty s >>= f = Read acc ty (s >>>= f)
  Branch p s11 s12 s2 >>= f = Branch (p >>= f) (s11 >>= f) (s12 >>= f) (s2 >>= f)
  TillT s s' >>= f = TillT (s >>= f) (s' >>= f)
  JumpPoint s s' >>= f = JumpPoint (s >>= f) (s' >>= f)
  -- Terms
  Nil >>= _ = Nil
  Lit n >>= _ = Lit n
  Add x y >>= f = Add (x >>= f) (y >>= f)
  Sub x y >>= f = Sub (x >>= f) (y >>= f)
  Mul x y >>= f = Mul (x >>= f) (y >>= f)
  Cons x y >>= f = Cons (x >>= f) (y >>= f)
  Len t >>= f = Len $ t >>= f
  Sum t >>= f = Sum $ t >>= f
  -- Predicates
  TT >>= _ = TT
  FF >>= _ = FF
  Neg p >>= f = Neg (p >>= f)
  And p q >>= f = And (p >>= f) (q >>= f)
  Or p q >>= f = Or (p >>= f) (q >>= f)
  Eq t u >>= f = Eq (t >>= f) (u >>= f)
  Less t u >>= f = Less (t >>= f) (u >>= f)

deriveEq1   ''Everything
deriveOrd1  ''Everything
deriveRead1 ''Everything
deriveShow1 ''Everything

instance Eq a   => Eq   (Everything a) where (==) = eq1
instance Ord a  => Ord  (Everything a) where compare = compare1
instance Show a => Show (Everything a) where showsPrec = showsPrec1
instance Read a => Read (Everything a) where readsPrec = readsPrec1
