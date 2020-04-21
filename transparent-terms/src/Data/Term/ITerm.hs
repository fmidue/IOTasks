{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Term.ITerm (
  ITerm,
  lit,
  ) where

import Data.List (nub)
import Data.Proxy
import Data.Dynamic (Typeable)
import Data.Either (either)

import Data.Term.Class
import Data.Term.Liftable
import Data.Term.AST
import Data.Environment.Class

data ITerm env v a = ITerm (AST v) [(v,Usage)] (env v -> a)

instance (VarEnv env v, Show v) => VarTerm (ITerm env v) v where
  variable' x = ITerm (UVar (x,Current)) [(x,Current)] (evalGetCurrent x)

instance (PVarEnv env v, Show v) => PVarTerm (ITerm env v) v where
  variableAll' x = ITerm (UVar (x,All)) [(x,All)] (evalGetAll x)

evalGetAll :: (PVarEnv env v, Show v, Typeable a) => v -> env v -> [a]
evalGetAll x d =
  either
    (error . printLookupError)
    id
    $ lookupAllAtType Proxy x d

evalGetCurrent :: (VarEnv env v, Show v, Typeable a) => v -> env v -> a
evalGetCurrent x d =
  either
    (error . printLookupError)
    id
    $ lookupAtType Proxy x d

instance SemTerm (ITerm env v) (env v) where
  evalTerm (ITerm _ _ f) = f

instance ClosedSemTerm (ITerm env v) where
  evalClosed (ITerm _ [] f) = f undefined -- assumption: if the ITerm does not contain variables f is non-strict in its parameter
  evalClosed _ = error "evalClosed: ITerm is not closed"

instance SynTerm (ITerm env v) (AST v) where
  viewTerm (ITerm ast _ _) = ast

instance Eq v => VarListTerm (ITerm env v) v where
  termVars (ITerm _ vs _) = nub $ map fst vs

instance (PVarTerm env v, PVarEnv env v, Show v) => UsageTerm (ITerm env v) v where
  varUsage = varInfo

varInfo :: ITerm env v a -> [(v,Usage)]
varInfo (ITerm _ vs _) = vs

lit :: (Eq v, Show a) => a -> ITerm env v a
lit x = embedT (x,show x)

instance Eq v => Liftable (ITerm env v) where
-- might need some sanity checks if publicly exposed
  embedT (x,s) = ITerm (Literal s) [] (const x)
  appT (ITerm ast1 vs1 f) (ITerm ast2 vs2 x) = ITerm (Node "app" [ast1, ast2]) (nub $ vs1 ++ vs2) (\e -> f e (x e))

  liftT (f,name) (ITerm ast vs eval) = ITerm (Node name [ast]) vs (f . eval)
  liftT2 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Node name [ast1, ast2]) (nub $ vs1 ++ vs2) (\d -> f (eval1 d) (eval2 d))
  liftTInfix ((*$),name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Infix ast1 name ast2) (nub $ vs1 ++ vs2) (\d -> eval1 d *$ eval2 d)
  liftT3 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2) (ITerm ast3 vs3 eval3)
    = ITerm (Node name [ast1, ast2, ast3]) (nub $ vs1 ++ vs2 ++ vs3) (\d -> f (eval1 d) (eval2 d) (eval3 d))

  unHO f = ITerm
    (funcAST f)
    (varInfo (f dummyClosedITerm))
    (\ d x -> evalTerm (f (dummyEvalITerm x)) d)

  unHO2 f = ITerm
    (funcAST2 f)
    (varInfo (f dummyClosedITerm dummyClosedITerm))
    (\ d x y -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y)) d)

  unHO3 f = ITerm
    (funcAST3 f)
    (varInfo (f dummyClosedITerm dummyClosedITerm dummyClosedITerm))
    (\ d x y z -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y) (dummyEvalITerm z)) d)

-- internal helpers
funcAST :: (ITerm env v a -> ITerm env v b) -> AST v
funcAST f = Lam ["x"] (viewTerm $ f (dummyViewITerm (Literal "x")))

funcAST2 :: (ITerm env v a -> ITerm env v b -> ITerm env v c) -> AST v
funcAST2 f = Lam ["x","y"] (viewTerm $ f (dummyViewITerm (Literal "x")) (dummyViewITerm (Literal "y")))

funcAST3 :: (ITerm env v a -> ITerm env v b -> ITerm env v c -> ITerm env v d) -> AST v
funcAST3 f = Lam ["x","y","z"] (viewTerm $ f (dummyViewITerm (Literal "x")) (dummyViewITerm (Literal "y")) (dummyViewITerm (Literal "z")))

-- usefull for when a ITerm with just a value is needed to evaluate a function on ITerms
dummyEvalITerm :: a -> ITerm env v a
dummyEvalITerm x = ITerm (error "dummyEvalITerm: no inspectable representaion") [] (const x)

-- usefull for when a ITerm with just an AST is needed to view a function on ITerms
dummyViewITerm :: AST v -> ITerm env v a
dummyViewITerm x = ITerm x [] (error "dummyViewITerm: evaluation not possible")

-- usefull for when a ITerm without variables is needed to deITermine the variables that occur in a function on ITerms
dummyClosedITerm :: ITerm env v a
dummyClosedITerm = ITerm (error "dummyClosedITerm: no inspectable representaion") [] (error "dummyClosedITerm: evaluation not possible")
