{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTest.Internal.Term (
  Term,
  getCurrent,
  getAll,
  evalTerm,
) where

import Test.IOTest.Internal.Environment

import Data.Dynamic
import           Data.List                      ( nub )

data Term a = Term { termVars :: [Varname], getTerm :: Environment -> a }

instance Functor Term where
  fmap f (Term vs e) = Term vs (f . e)

instance Applicative Term where
  pure x = Term [] $ const x
  Term vs1 fab <*> Term vs2 fa = Term (nub $ vs1 ++ vs2) $ \d -> fab d $ fa d

evalTerm :: Term a -> Environment -> a
evalTerm = getTerm

getCurrent :: Typeable a => Varname -> Term a
getCurrent x = Term [x] $ \d ->
  let xs = evalTerm (getAll x) d
  in if (not . null) xs
    then last xs
    else error $ "getCurrent: no values stored for " <> x

getAll :: Typeable a => Varname -> Term [a]
getAll x = Term [x] $ \d ->
  let mVs = lookupNameAtType x d in
  case mVs of
    Left e -> error $ printLookupError e
    Right vs -> vs

instance HasVariables (Term a) where
  vars = termVars
