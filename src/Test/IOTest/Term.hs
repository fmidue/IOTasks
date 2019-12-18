{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTest.Term (
  Term,
  EvalTerm,
  ViewTerm(..),
  LengthTermSym(..),
  EqTermSym(..),
  SumTermSym(..),
  AST(..),
  TermVars(..),
  getCurrent,
  getAll,
  evalTerm,
  ApplicativeTerm,
) where

import Test.IOTest.Environment

import Data.Dynamic
import Data.Proxy
import Data.List ( nub )
import Control.Applicative ( liftA2 )

-- terms that can express acces to variables
class Term t where
  getAll :: forall a. Typeable a => Varname -> t [a]
  getCurrent :: forall a. Typeable a => Varname -> t a

-- terms that can be evaluated to Haskell values using an Environment
class Term t => EvalTerm t where
  evalTerm :: t a -> Environment -> a

-- terms that have a printable representation of their AST
class Term t => ViewTerm t where
  viewTerm :: t a -> AST

data AST = Node String [AST] | Leaf String

-- terms that might have a printable representation of their AST
class Term t => PartialViewTerm t where
  viewTermMaybe :: t a -> Maybe AST

class TermVars t where
  termVars :: t a -> [Varname]

---

data ApplicativeTerm a = ApplicativeTerm { apTermVars :: [Varname], getTerm :: Environment -> a }

instance Functor ApplicativeTerm where
  fmap f (ApplicativeTerm vs g) = ApplicativeTerm vs $ f . g

instance Applicative ApplicativeTerm where
  pure x = ApplicativeTerm [] $ const x
  (ApplicativeTerm xs fab) <*> (ApplicativeTerm ys fa) = ApplicativeTerm (nub $ xs ++ ys) $ fab <*> fa

instance Term ApplicativeTerm where
  getAll x = ApplicativeTerm [x] $ \d ->
    let mVs = lookupNameAtType Proxy x d in
    case mVs of
      Left e -> error $ printLookupError e
      Right vs -> vs

  getCurrent x =
    let vs = getAll x
    in last <$> vs

instance EvalTerm ApplicativeTerm where
  evalTerm = getTerm

instance TermVars ApplicativeTerm where
  termVars = apTermVars

class LengthTermSym t where
  len :: t [a] -> t Int

class EqTermSym t where
  eq :: Eq a => t a -> t a -> t Bool

class SumTermSym t where
  sum :: Num a => t [a] -> t a

instance LengthTermSym ApplicativeTerm where
  len = fmap length

instance EqTermSym ApplicativeTerm where
  eq = liftA2 (==)

newtype S a = S { unS :: AST }

instance ViewTerm S where
  viewTerm = unS

instance Term S where
  getAll x = S $ Leaf $ x ++ "_A"
  getCurrent x = S $ Leaf $ x ++ "_C"

instance LengthTermSym S where
  len t = S $ Node "len" [unS t]

instance EqTermSym S where
  eq x y = S $ Node "eq" [unS x, unS y]

data HybridTerm a = HT { _evalHybrid :: ApplicativeTerm a
                       , viewHybrid :: Maybe (S a)
                       }

instance Term HybridTerm where
  getAll x = HT (getAll x) (Just $ getAll x)
  getCurrent x = HT (getCurrent x) (Just $ getCurrent x)

instance LengthTermSym HybridTerm where
  len (HT ex vx) = HT (len ex) (len <$> vx)

instance EqTermSym HybridTerm where
  eq (HT ex vx) (HT ey vy) = HT (eq ex ey) (eq <$> vx <*> vy)

instance Functor HybridTerm where
  fmap f (HT fa _) = HT (fmap f fa) Nothing

instance Applicative HybridTerm where
  pure a = HT (pure a) Nothing
  HT fab _ <*> HT fa _ = HT (fab <*> fa) Nothing

instance EvalTerm HybridTerm where
  evalTerm (HT t _) = evalTerm t

instance PartialViewTerm HybridTerm where
  viewTermMaybe = fmap unS . viewHybrid
