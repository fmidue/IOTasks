{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Test.IOTest.Term (
  Term,
  AST(..),
  evalTerm,
  evalClosed,
  isClosed,
  viewTerm,
  termVars,
  --
  getCurrent,
  getAll,
  lit,
  liftT,
  liftT2,
  liftT3,
  unHO,
  unHO2,
  unHO3,
  --
  sum,
  length,
  (+),
  (-),
  (*),
  (==),
  (&&),
  (>),
  (<),
  foldr,
  map,
  last,
  init,
  mod,
  filter,
  reverse,
  elem,
  notElem,
  all,
  null,
  not,
) where

import qualified Prelude
import Prelude hiding (not,null,sum,length,foldr,(+), map, (==), (>), (<), last, init, (&&), mod, filter, reverse, elem, notElem, all, (-), (*))

import Test.IOTest.Environment

import Data.Dynamic
import Data.Proxy
import Data.List ( nub )

data Term a = Term AST [Varname] (Environment -> a)

evalTerm :: Term a -> Environment -> a
evalTerm (Term _ _ f) = f

evalClosed :: Term a -> a
evalClosed (Term _ [] f) = f undefined -- assumption: if the term does not contain variables f is non-strict in its parameter
evalClosed _ = error "evalClosed: term is not closed"

isClosed :: Term a -> Bool
isClosed = Prelude.null . termVars

viewTerm :: Term a -> AST
viewTerm (Term ast _ _) = ast

termVars :: Term a -> [Varname]
termVars (Term _ vs _) = vs

-- Printable representation of terms
data AST
  = Node String [AST]
  | Infix AST String AST
  | Leaf String
  | Lam [Varname] AST
  deriving Show

printAST :: AST -> String
printAST = printAST' "" ""
  where
    printAST' o c (Node f xs) = o ++ f ++ " " ++ unwords (fmap (printAST' "(" ")") xs) ++ c
    printAST' o c (Infix x op y) = o ++ printAST x ++ " " ++ op ++ " " ++ printAST y ++ c
    printAST' _ _ (Leaf x) = x
    printAST' o c (Lam vs b) = o ++ "\\" ++ unwords vs ++ " -> " ++ printAST b ++ c

-- construction of terms
getAll :: Typeable a => Varname -> Term [a]
getAll x = Term (Leaf $ x ++ "_A") [x] evalGetAll where
  evalGetAll d =
    let mVs = lookupNameAtType Proxy x d in
    case mVs of
      Left e -> error $ printLookupError e
      Right vs -> vs

getCurrent :: Typeable a => Varname -> Term a
getCurrent x = Term (Leaf $ x ++ "_C") [x] evalGetCurrent where
  evalGetCurrent d = Prelude.last $ evalTerm (getAll x) d

lit :: Show a => a -> Term a
lit x = Term (Leaf $ show x) [] (const x)

liftT :: (a -> b, String) -> Term a -> Term b
liftT (f,name) (Term ast vs eval) = Term (Node name [ast]) vs (f . eval)

liftT2 :: (a -> b -> c, String) -> Term a -> Term b -> Term c
liftT2 (f,name) (Term ast1 vs1 eval1) (Term ast2 vs2 eval2)  = Term (Node name [ast1, ast2]) (nub $ vs1 ++ vs2) (\d -> f (eval1 d) (eval2 d))

liftTInfix :: (a -> b -> c, String) -> Term a -> Term b -> Term c
liftTInfix ((*$),name) (Term ast1 vs1 eval1) (Term ast2 vs2 eval2)  = Term (Infix ast1 name ast2) (nub $ vs1 ++ vs2) (\d -> eval1 d *$ eval2 d)

liftT3 :: (a -> b -> c -> d, String) -> Term a -> Term b -> Term c -> Term d
liftT3 (f,name) (Term ast1 vs1 eval1) (Term ast2 vs2 eval2) (Term ast3 vs3 eval3)
  = Term (Node name [ast1, ast2, ast3]) (nub $ vs1 ++ vs2 ++ vs3) (\d -> f (eval1 d) (eval2 d) (eval3 d))

unHO :: (Term a -> Term b) -> Term (a -> b)
unHO f = Term
  (funcAST f)
  (termVars (f dummyClosedTerm))
  (\ d x -> evalTerm (f (dummyEvalTerm x)) d)

unHO2 :: (Term a -> Term b -> Term c) -> Term (a -> b -> c)
unHO2 f = Term
  (funcAST2 f)
  (termVars (f dummyClosedTerm dummyClosedTerm))
  (\ d x y -> evalTerm (f (dummyEvalTerm x) (dummyEvalTerm y)) d)

unHO3 :: (Term a -> Term b -> Term c -> Term d) -> Term (a -> b -> c -> d)
unHO3 f = Term
  (funcAST3 f)
  (termVars (f dummyClosedTerm dummyClosedTerm dummyClosedTerm))
  (\ d x y z -> evalTerm (f (dummyEvalTerm x) (dummyEvalTerm y) (dummyEvalTerm z)) d)

-- internal helpers
funcAST :: (Term a -> Term b) -> AST
funcAST f = Lam ["x"] (viewTerm $ f (dummyViewTerm (Leaf "x")))

funcAST2 :: (Term a -> Term b -> Term c) -> AST
funcAST2 f = Lam ["x","y"] (viewTerm $ f (dummyViewTerm (Leaf "x")) (dummyViewTerm (Leaf "y")))

funcAST3 :: (Term a -> Term b -> Term c -> Term d) -> AST
funcAST3 f = Lam ["x","y","z"] (viewTerm $ f (dummyViewTerm (Leaf "x")) (dummyViewTerm (Leaf "y")) (dummyViewTerm (Leaf "z")))

-- usefull for when a term with just a value is needed to evaluate a function on terms
dummyEvalTerm :: a -> Term a
dummyEvalTerm x = Term (error "dummyEvalTerm: no inspectable representaion") [] (const x)

-- usefull for when a term with just an AST is needed to view a function on terms
dummyViewTerm :: AST -> Term a
dummyViewTerm x = Term x [] (error "dummyViewTerm: evaluation not possible")

-- usefull for when a term without variables is needed to determine the variables that occur in a function on terms
dummyClosedTerm :: Term a
dummyClosedTerm = Term (error "dummyClosedTerm: no inspectable representaion") [] (error "dummyClosedTerm: evaluation not possible")

-- example embeddings
sum :: (Foldable t, Num a) => Term (t a) -> Term a
sum = liftT (Prelude.sum,"sum")

(+) :: Num a => Term a -> Term a -> Term a
(+) = liftTInfix ((Prelude.+), "+")

(-) :: Num a => Term a -> Term a -> Term a
(-) = liftTInfix ((Prelude.-), "-")

(*) :: Num a => Term a -> Term a -> Term a
(*) = liftTInfix ((Prelude.*), "*")

mod :: Integral a => Term a -> Term a -> Term a
mod = liftT2 (Prelude.mod, "mod")

(==) :: Eq a => Term a -> Term a -> Term Bool
(==) = liftTInfix ((Prelude.==), "==")

(&&) :: Term Bool -> Term Bool -> Term Bool
(&&) = liftTInfix ((Prelude.&&), "&&")

(>) :: Ord a => Term a -> Term a -> Term Bool
(>) = liftTInfix ((Prelude.>), ">")

(<) :: Ord a => Term a -> Term a -> Term Bool
(<) = liftTInfix ((Prelude.<), "<")

length :: Foldable t => Term (t a) -> Term Int
length = liftT (Prelude.length,"length")

foldr :: Foldable t => (Term a -> Term b -> Term b) -> Term b -> Term (t a) -> Term b
foldr = liftT3 (Prelude.foldr,"foldr") . unHO2

map :: (Term a -> Term b) -> Term [a] -> Term [b]
map = liftT2 (Prelude.map, "map") . unHO

filter :: (Term a -> Term Bool) -> Term [a] -> Term [a]
filter = liftT2 (Prelude.filter, "filter") . unHO

init :: Term [a] -> Term [a]
init = liftT (Prelude.init, "init")

last :: Term [b] -> Term b
last = liftT (Prelude.last, "last")

reverse :: Term [a] -> Term [a]
reverse = liftT (Prelude.reverse, "reverse")

elem :: Eq a => Term a -> Term [a] -> Term Bool
elem = liftT2 (Prelude.elem,"elem")

notElem :: Eq a => Term a -> Term [a] -> Term Bool
notElem = liftT2 (Prelude.notElem,"notElem")

all :: Foldable t => (Term a -> Term Bool) -> Term (t a) -> Term Bool
all = liftT2 (Prelude.all, "all") . unHO

null :: Foldable t => Term (t a) -> Term Bool
null = liftT (Prelude.null,"null")

not :: Term Bool -> Term Bool
not = liftT (Prelude.not, "not")
