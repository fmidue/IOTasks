module Test.IOTasks.Term.ATerm (
  ATerm,
) where

import Data.List (nub)
import Data.Proxy

import Test.IOTasks.Term
import Test.IOTasks.Environment (Varname, Environment, lookupNameAtType, printLookupError)

data ATerm a = ATerm { aTermVars :: [Varname], getTerm :: Environment -> a }

instance Functor ATerm where
  fmap f (ATerm vs e) = ATerm vs (f . e)

instance Applicative ATerm where
  pure x = ATerm [] $ const x
  ATerm vs1 fab <*> ATerm vs2 fa = ATerm (nub $ vs1 ++ vs2) $ \d -> fab d $ fa d

instance Term ATerm where
  getCurrent' x = ATerm [x] $ \d ->
    let xs = getTerm (getAll x) d
    in if (not . null) xs
      then last xs
      else error $ "getCurrent: no values stored for " <> x <> " in " <> show d

  getAll' x = ATerm [x] $ \d ->
    let mVs = lookupNameAtType Proxy x d in
    case mVs of
      Left e -> error $ printLookupError e
      Right vs -> vs

instance SemTerm ATerm where
  evalTerm = getTerm

instance TermVars ATerm where
  termVars = aTermVars
