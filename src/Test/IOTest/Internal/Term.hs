{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTest.Internal.Term (
  Term,
  update,
  getCurrent,
  getAll,
  evalTerm,
  epsilon,
  isEpsilon
) where

import Test.IOTest.Internal.Context (Context)

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Data.Maybe

newtype Term v s a = Term { getTerm :: MaybeT (State (Context v s)) a }
  deriving (Functor, Applicative) via (MaybeT (State (Context v s)))

evalTerm :: Term v s a -> Context v s -> a
evalTerm t = fromMaybe (error "Can not evaluate epsilon!") . (evalState . runMaybeT $ getTerm t)

update :: Eq v => v -> s -> Term v s ()
update x v = Term $ MaybeT $ Just <$> modify (map (x `addValue` v))
  where
    addValue x' v' (y,vs') = if y == x' then (y,vs' ++ [v']) else (y,vs')

epsilon :: Term v s a
epsilon = Term $ MaybeT $ return Nothing

isEpsilon :: Term v s a -> Bool
isEpsilon t = isNothing $ evalState (runMaybeT $ getTerm t) []

getCurrent :: Eq v => v -> Term v a a
getCurrent x = last <$> getAll x

getAll :: Eq v => v -> Term v a [a]
getAll x = Term $ MaybeT $ Just <$> gets (fromMaybe (error "lookup failed!") . lookup x)
