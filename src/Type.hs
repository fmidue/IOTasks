{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Type where

import qualified Language as Surface
import Language (VarName,NumType)

import Bound

import Control.Monad (ap)

import Data.Functor.Classes
import Data.Deriving (deriveShow1)

data Position = First | Second deriving (Eq,Ord,Read,Show)

type Spec = Everything
type Value = Everything
type Fun = Everything
type Pred = Everything
type GlobalV = Everything

data Everything a
  -- Variables
  = V a
  -- Spec
  -- first parameter is the name of the global accumulator
  | Read VarName NumType (Scope () Everything a)
  | Write [Fun a] (Spec a)
  | TillT (Spec a) (Spec a)
  | Branch (Pred a) (Spec a) (Spec a) (Spec a)
  | InternalT (Spec a)
  | Nop
  | JumpPoint (Spec a) (Spec a)
  -- Values
  | Lit Int
  | Cons (Value a) (Value a)
  | Nil
  -- Function
  | UFun Function (Value a)
  | BFun Function (Value a) (Value a)
  | DoNothing
  -- Predicates
  | UPred Predicate (Value a)
  | BPred Predicate (Value a) (Value a)
  deriving (Functor, Foldable, Traversable)

-- Functions evaluating to Int
data Function
  = UIntF (Int -> Int)
  | BIntF (Int -> Int -> Int)
  | UListF ([Int] -> Int)
  | BListF ([Int] -> [Int] -> Int)
  | MixedF ([Int] -> Int -> Int)

-- Functions evaluating to Bool, i.e., predicates
data Predicate
  = UIntP (Int -> Bool)
  | BIntP (Int -> Int -> Bool)
  | UListP ([Int] -> Bool)
  | BListP ([Int] -> [Int] -> Bool)
  | MixedP ([Int] -> Int -> Bool)

andThen :: Spec a -> Spec a -> Spec a
andThen (Read xs ty s2) s' =
  let s2' = toScope (fromScope s2 `andThen` (F <$> s'))
  in Read xs ty s2'
andThen (Write s1 s2) s' = Write s1 $ s2 `andThen` s'
andThen (TillT s1 s2) s' = TillT s1 $ s2 `andThen` s'
andThen (Branch p s11 s12 s2) s' = Branch p s11 s12 $ s2 `andThen` s'
andThen (InternalT s) s' = InternalT $ s `andThen` s'
andThen Nop s' = s'
andThen (JumpPoint s1 s2) s' = JumpPoint s1 $ s2 `andThen` s'
andThen _ _ = error "Not a spec"

unsugar :: Surface.Specification -> Spec VarName
unsugar (Surface.Seq (Surface.ReadInput x ty xs) s') = Read xs ty $ abstract1 x (unsugar s')
unsugar (Surface.Seq (Surface.WriteOutput fs) s') = Write (unsugarFunc <$> fs) (unsugar s')
unsugar (Surface.Seq (Surface.TillT s) s') = TillT (unsugar s) (unsugar s')
unsugar (Surface.Seq (Surface.Branch (Surface.UIntP p x) s1 s2) s') = Branch (UPred (UIntP p) (V x)) (unsugar s1) (unsugar s2) (unsugar s')
unsugar (Surface.Seq (Surface.Branch (Surface.BIntP p (x,y)) s1 s2) s') = Branch (BPred (BIntP p) (V x) (V y)) (unsugar s1) (unsugar s2) (unsugar s')
unsugar (Surface.Seq (Surface.Branch (Surface.UListP p x) s1 s2) s') = Branch (UPred (UListP p) (V x)) (unsugar s1) (unsugar s2) (unsugar s')
unsugar (Surface.Seq (Surface.Branch (Surface.BListP p (x,y)) s1 s2) s') = Branch (BPred (BListP p) (V x) (V y)) (unsugar s1) (unsugar s2) (unsugar s')
unsugar (Surface.Seq (Surface.Branch (Surface.MixedP p (x,y)) s1 s2) s') = Branch (BPred (MixedP p) (V x) (V y)) (unsugar s1) (unsugar s2) (unsugar s')
unsugar Surface.T = InternalT Nop
unsugar Surface.Nop = Nop
unsugar x = unsugar $ Surface.Seq x Surface.Nop

unsugarFunc :: Surface.Function -> Fun VarName
unsugarFunc (Surface.UIntF f x) = UFun (UIntF f) (V x)
unsugarFunc (Surface.BIntF f (x,y)) = BFun (BIntF f) (V x) (V y)
unsugarFunc (Surface.UListF f x) = UFun (UListF f) (V x)
unsugarFunc (Surface.BListF f (x,y)) = BFun (BListF f) (V x) (V y)
unsugarFunc (Surface.MixedF f (x,y)) = BFun (MixedF f) (V x) (V y)
unsugarFunc Surface.Optional = DoNothing

instance Applicative Everything where pure = V; (<*>) = ap
instance Monad Everything where
  return = V
  -- Variables
  V a >>= f = f a
  -- Spec
  InternalT s >>= f = InternalT (s >>= f)
  Nop >>= _ = Nop
  Write t s >>= f = Write ((>>= f) <$> t) (s >>= f)
  Read acc ty s >>= f = Read acc ty (s >>>= f)
  Branch p s11 s12 s2 >>= f = Branch (p >>= f) (s11 >>= f) (s12 >>= f) (s2 >>= f)
  TillT s s' >>= f = TillT (s >>= f) (s' >>= f)
  JumpPoint s s' >>= f = JumpPoint (s >>= f) (s' >>= f)
  -- Values
  Nil >>= _ = Nil
  Lit n >>= _ = Lit n
  Cons x y >>= f = Cons (x >>= f) (y >>= f)
  -- Functions
  (UFun g x) >>= f = UFun g $ x >>= f
  (BFun g x y) >>= f = BFun g (x >>= f) (y >>= f)
  (UPred p x) >>= f = UPred p $ x >>= f
  (BPred p x y) >>= f = BPred p (x >>= f) (y >>= f)
  DoNothing >>= _ = DoNothing

instance Show Function where
  show (UIntF _) = "*Int -> Int*"
  show (BIntF _) = "*Int -> Int -> Int*"
  show (UListF _) = "*[Int] -> Int*"
  show (BListF _) = "*[Int] -> [Int] -> Int*"
  show (MixedF _) = "*[Int] -> Int -> Int*"

instance Show Predicate where
  show (UIntP _) = "*Int -> Bool*"
  show (BIntP _) = "*Int -> Int -> Bool*"
  show (UListP _) = "*[Int] -> Bool*"
  show (BListP _) = "*[Int] -> [Int] -> Bool*"
  show (MixedP _) = "*[Int] -> Int -> Bool*"

deriveShow1 ''Everything

instance Show a => Show (Everything a) where showsPrec = showsPrec1
