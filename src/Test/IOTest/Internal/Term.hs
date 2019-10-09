{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTest.Internal.Term (
  Term,
  --update,
  getCurrent,
  getAll,
  evalTerm,
) where

import Test.IOTest.Internal.Environment

import Data.Functor.Identity (Identity(..))
import Control.Monad.Reader

import Data.Dynamic

newtype Term a = Term { getTerm :: Environment -> a }
  deriving (Functor, Applicative) via (Reader Environment)

evalTerm :: Term a -> Environment -> a
evalTerm = getTerm

getCurrent :: Typeable a => Varname -> Term a
getCurrent x = Term $ \d ->
  let xs = evalTerm (getAll x) d
  in if (not . null) xs
    then last xs
    else error $ "getCurrent: no values stored for " <> x

getAll :: Typeable a => Varname -> Term [a]
getAll x = Term $ \d ->
  let mVs = lookupNameAtType x d in
  case mVs of
    Left e -> error $ printLookupError e
    Right vs -> vs
