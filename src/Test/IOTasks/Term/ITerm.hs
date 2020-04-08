{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTasks.Term.ITerm (
  ITerm,
  evalClosed,
  isClosed,
  --
  lit,
  liftT,
  liftT2,
  liftTInfix,
  liftT3,
  unHO,
  unHO2,
  unHO3,
) where

import Data.List (nub)
import Data.Proxy
import Data.Dynamic (Typeable)

import Test.IOTasks.Term
import Test.IOTasks.Environment (Varname, Environment, lookupAllAtType, lookupLastAtType, printLookupError)

data ITerm v a = ITerm (AST v) [v] (Environment -> a)

instance Term ITerm where
  getCurrent' x = ITerm (Var (addUsageInfo x C)) [addUsageInfo x C] (evalGetCurrent x)
  getAll' x = ITerm (Var (addUsageInfo x A)) [addUsageInfo x A] (evalGetAll x)

evalGetAll :: Typeable a => Varname -> Environment -> [a]
evalGetAll x d =
  let mv = lookupAllAtType Proxy x d in
  case mv of
    Left e -> error $ printLookupError e
    Right v -> v

evalGetCurrent :: Typeable a => Varname -> Environment -> a
evalGetCurrent x d =
  let mv = lookupLastAtType Proxy x d in
  case mv of
    Left e -> error $ printLookupError e
    Right v -> v

instance SemTerm ITerm where
  evalTerm (ITerm _ _ f) = f

instance SynTerm ITerm where
  viewTerm (ITerm ast _ _) = ast

instance TermVars ITerm where
  termVars (ITerm _ vs _) = vs

evalClosed :: ITerm v a -> a
evalClosed (ITerm _ [] f) = f undefined -- assumption: if the ITerm does not contain variables f is non-strict in its parameter
evalClosed _ = error "evalClosed: ITerm is not closed"

isClosed :: ITerm v a -> Bool
isClosed = Prelude.null . termVars

lit :: Show a => a -> ITerm v a
lit x = embed x (Literal $ show x)

-- might need some sanity checks if publicly exposed
embed :: a -> AST v -> ITerm v a
embed x ast = ITerm ast [] (const x)

liftT :: (a -> b, String) -> ITerm v a -> ITerm v b
liftT (f,name) (ITerm ast vs eval) = ITerm (Node name [ast]) vs (f . eval)

liftT2 :: Eq v => (a -> b -> c, String) -> ITerm v a -> ITerm v b -> ITerm v c
liftT2 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Node name [ast1, ast2]) (nub $ vs1 ++ vs2) (\d -> f (eval1 d) (eval2 d))

liftTInfix :: Eq v => (a -> b -> c, String) -> ITerm v a -> ITerm v b -> ITerm v c
liftTInfix ((*$),name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Infix ast1 name ast2) (nub $ vs1 ++ vs2) (\d -> eval1 d *$ eval2 d)

liftT3 :: Eq v => (a -> b -> c -> d, String) -> ITerm v a -> ITerm v b -> ITerm v c -> ITerm v d
liftT3 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2) (ITerm ast3 vs3 eval3)
  = ITerm (Node name [ast1, ast2, ast3]) (nub $ vs1 ++ vs2 ++ vs3) (\d -> f (eval1 d) (eval2 d) (eval3 d))

-- | usfull for lifting higher order functions.
--
-- E.g. @'liftT2' ('map', "map") . 'unHO'@ lifts @'map'@
-- to type @('ITerm' a -> 'ITerm' b) -> 'ITerm' [a] -> 'ITerm' [b]@
--
-- while doing just @'liftT2' ('map', "map")@ would result in type @'ITerm'(a -> b) -> 'ITerm' [a] -> 'ITerm' [b]@
unHO :: (ITerm v a -> ITerm v b) -> ITerm v (a -> b)
unHO f = ITerm
  (funcAST f)
  (termVars (f dummyClosedITerm))
  (\ d x -> evalTerm (f (dummyEvalITerm x)) d)

unHO2 :: (ITerm v a -> ITerm v b -> ITerm v c) -> ITerm v (a -> b -> c)
unHO2 f = ITerm
  (funcAST2 f)
  (termVars (f dummyClosedITerm dummyClosedITerm))
  (\ d x y -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y)) d)

unHO3 :: (ITerm v a -> ITerm v b -> ITerm v c -> ITerm v d) -> ITerm v (a -> b -> c -> d)
unHO3 f = ITerm
  (funcAST3 f)
  (termVars (f dummyClosedITerm dummyClosedITerm dummyClosedITerm))
  (\ d x y z -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y) (dummyEvalITerm z)) d)

-- internal helpers
funcAST :: (ITerm v a -> ITerm v b) -> AST v
funcAST f = Lam ["x"] (viewTerm $ f (dummyViewITerm (Literal "x")))

funcAST2 :: (ITerm v a -> ITerm v b -> ITerm v c) -> AST v
funcAST2 f = Lam ["x","y"] (viewTerm $ f (dummyViewITerm (Literal "x")) (dummyViewITerm (Literal "y")))

funcAST3 :: (ITerm v a -> ITerm v b -> ITerm v c -> ITerm v d) -> AST v
funcAST3 f = Lam ["x","y","z"] (viewTerm $ f (dummyViewITerm (Literal "x")) (dummyViewITerm (Literal "y")) (dummyViewITerm (Literal "z")))

-- usefull for when a ITerm with just a value is needed to evaluate a function on ITerms
dummyEvalITerm :: a -> ITerm v a
dummyEvalITerm x = ITerm (error "dummyEvalITerm: no inspectable representaion") [] (const x)

-- usefull for when a ITerm with just an AST is needed to view a function on ITerms
dummyViewITerm :: AST v -> ITerm v a
dummyViewITerm x = ITerm x [] (error "dummyViewITerm: evaluation not possible")

-- usefull for when a ITerm without variables is needed to deITermine the variables that occur in a function on ITerms
dummyClosedITerm :: ITerm v a
dummyClosedITerm = ITerm (error "dummyClosedITerm: no inspectable representaion") [] (error "dummyClosedITerm: evaluation not possible")
