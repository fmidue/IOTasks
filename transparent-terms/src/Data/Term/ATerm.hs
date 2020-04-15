{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Term.ATerm (
  ATerm,
) where

import Data.List (nub)
import Data.Proxy

import Data.Term.Class
import Data.Environment.Class

data ATerm env v a = ATerm { aTermVars :: [(v,Usage)], getTerm :: env v -> a }

instance Functor (ATerm env v) where
  fmap f (ATerm vs e) = ATerm vs (f . e)

instance Eq v => Applicative (ATerm env v) where
  pure x = ATerm [] $ const x
  ATerm vs1 fab <*> ATerm vs2 fa = ATerm (nub $ vs1 ++ vs2) $ \d -> fab d $ fa d

instance (VarEnv env v, Show v) => VarTerm (ATerm env v) v where
  variable' x = ATerm [(x,Current)] $ \d ->
    let mv = lookupAtType Proxy x d in
    case mv of
      Left e -> error $ printLookupError e
      Right vs -> vs

instance (PVarEnv env v, Show v) => PVarTerm (ATerm env v) v where
  variableAll' x = ATerm [(x,All)] $ \d ->
    let mVs = lookupAllAtType Proxy x d in
    case mVs of
      Left e -> error $ printLookupError e
      Right vs -> vs

instance SemTerm (ATerm env v) (env v) where
  evalTerm = getTerm

instance Eq v => VarListTerm (ATerm env v) v where
  termVars = nub . map fst . aTermVars

instance (PVarEnv env v, Show v) => UsageTerm (ATerm env v) v where
  varUsage = aTermVars
