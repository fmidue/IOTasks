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
import Test.IOTasks.Environment (Varname, Environment, lookupNameAtType, printLookupError)

data ITerm a = ITerm AST [Varname] (Environment -> a)

instance Term ITerm where
  getAll' x = ITerm (Leaf $ x ++ "_A") [x] (evalGetAll x)

  getCurrent' x = ITerm (Leaf $ x ++ "_C") [x] (last . evalGetAll x)

evalGetAll :: Typeable a => Varname -> Environment -> [a]
evalGetAll x d =
  let mVs = lookupNameAtType Proxy x d in
  case mVs of
    Left e -> error $ printLookupError e
    Right vs -> vs

instance SemTerm ITerm where
  evalTerm (ITerm _ _ f) = f

instance SynTerm ITerm where
  viewTerm (ITerm ast _ _) = ast

instance TermVars ITerm where
  termVars (ITerm _ vs _) = vs

evalClosed :: ITerm a -> a
evalClosed (ITerm _ [] f) = f undefined -- assumption: if the ITerm does not contain variables f is non-strict in its parameter
evalClosed _ = error "evalClosed: ITerm is not closed"

isClosed :: ITerm a -> Bool
isClosed = Prelude.null . termVars

lit :: Show a => a -> ITerm a
lit x = embed x (Leaf $ show x)

-- might need some sanity checks if publicly exposed
embed :: a -> AST -> ITerm a
embed x ast = ITerm ast [] (const x)

liftT :: (a -> b, String) -> ITerm a -> ITerm b
liftT (f,name) (ITerm ast vs eval) = ITerm (Node name [ast]) vs (f . eval)

liftT2 :: (a -> b -> c, String) -> ITerm a -> ITerm b -> ITerm c
liftT2 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Node name [ast1, ast2]) (nub $ vs1 ++ vs2) (\d -> f (eval1 d) (eval2 d))

liftTInfix :: (a -> b -> c, String) -> ITerm a -> ITerm b -> ITerm c
liftTInfix ((*$),name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2)  = ITerm (Infix ast1 name ast2) (nub $ vs1 ++ vs2) (\d -> eval1 d *$ eval2 d)

liftT3 :: (a -> b -> c -> d, String) -> ITerm a -> ITerm b -> ITerm c -> ITerm d
liftT3 (f,name) (ITerm ast1 vs1 eval1) (ITerm ast2 vs2 eval2) (ITerm ast3 vs3 eval3)
  = ITerm (Node name [ast1, ast2, ast3]) (nub $ vs1 ++ vs2 ++ vs3) (\d -> f (eval1 d) (eval2 d) (eval3 d))

-- | usfull for lifting higher order functions.
--
-- E.g. @'liftT2' ('map', "map") . 'unHO'@ lifts @'map'@
-- to type @('ITerm' a -> 'ITerm' b) -> 'ITerm' [a] -> 'ITerm' [b]@
--
-- while doing just @'liftT2' ('map', "map")@ would result in type @'ITerm'(a -> b) -> 'ITerm' [a] -> 'ITerm' [b]@
unHO :: (ITerm a -> ITerm b) -> ITerm (a -> b)
unHO f = ITerm
  (funcAST f)
  (termVars (f dummyClosedITerm))
  (\ d x -> evalTerm (f (dummyEvalITerm x)) d)

unHO2 :: (ITerm a -> ITerm b -> ITerm c) -> ITerm (a -> b -> c)
unHO2 f = ITerm
  (funcAST2 f)
  (termVars (f dummyClosedITerm dummyClosedITerm))
  (\ d x y -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y)) d)

unHO3 :: (ITerm a -> ITerm b -> ITerm c -> ITerm d) -> ITerm (a -> b -> c -> d)
unHO3 f = ITerm
  (funcAST3 f)
  (termVars (f dummyClosedITerm dummyClosedITerm dummyClosedITerm))
  (\ d x y z -> evalTerm (f (dummyEvalITerm x) (dummyEvalITerm y) (dummyEvalITerm z)) d)

-- internal helpers
funcAST :: (ITerm a -> ITerm b) -> AST
funcAST f = Lam ["x"] (viewTerm $ f (dummyViewITerm (Leaf "x")))

funcAST2 :: (ITerm a -> ITerm b -> ITerm c) -> AST
funcAST2 f = Lam ["x","y"] (viewTerm $ f (dummyViewITerm (Leaf "x")) (dummyViewITerm (Leaf "y")))

funcAST3 :: (ITerm a -> ITerm b -> ITerm c -> ITerm d) -> AST
funcAST3 f = Lam ["x","y","z"] (viewTerm $ f (dummyViewITerm (Leaf "x")) (dummyViewITerm (Leaf "y")) (dummyViewITerm (Leaf "z")))

-- usefull for when a ITerm with just a value is needed to evaluate a function on ITerms
dummyEvalITerm :: a -> ITerm a
dummyEvalITerm x = ITerm (error "dummyEvalITerm: no inspectable representaion") [] (const x)

-- usefull for when a ITerm with just an AST is needed to view a function on ITerms
dummyViewITerm :: AST -> ITerm a
dummyViewITerm x = ITerm x [] (error "dummyViewITerm: evaluation not possible")

-- usefull for when a ITerm without variables is needed to deITermine the variables that occur in a function on ITerms
dummyClosedITerm :: ITerm a
dummyClosedITerm = ITerm (error "dummyClosedITerm: no inspectable representaion") [] (error "dummyClosedITerm: evaluation not possible")
