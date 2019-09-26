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
import Test.IOTest.Utils

import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Maybe
import Data.Dynamic
import Data.Proxy

newtype Term a = Term { getTerm :: MaybeT (State Environment) a }
  deriving (Functor, Applicative) via (MaybeT (State Environment))

evalTerm :: Term a -> Environment -> a
evalTerm t = fromMaybe (error "Can not evaluate epsilon!") . (evalState . runMaybeT $ getTerm t)

--TODO: What is this good for?
-- update :: Eq v => v -> s -> Term ()
-- update x v = Term $ MaybeT $ Just <$> modify (map (x `addValue` v))
--   where
--     addValue x' v' (y,vs') = if y == x' then (y,vs' ++ [v']) else (y,vs')

epsilon :: Term a
epsilon = Term $ MaybeT $ return Nothing

isEpsilon :: Term a -> Bool
isEpsilon t = isNothing $ evalState (runMaybeT $ getTerm t) []

getCurrent :: forall s a . (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Term a
getCurrent p x =
  let vs = getAll p x
  in if not . null $ evalState (runMaybeT $ getTerm vs) []
    then last <$> vs
    else error $ "getCurrent: no values stored for " <> x

getAll :: forall s a . (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Term [a]
getAll p x = Term . MaybeT $ Just <$> do
  mVs <- gets $ lookupNameAtType p x
  case mVs of
    Left e -> error $ printLookupError e
    Right vs -> return vs
