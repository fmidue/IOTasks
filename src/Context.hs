{-# LANGUAGE TupleSections #-}
module Context where

import Language as L
import Type as T

import Data.List (nub)
import Data.Maybe (fromJust)

newtype Context = Context [(VarName,[Int])] deriving Show

freshContext :: HasVariables a => a -> Context
freshContext s = Context ((,[]) <$> vars s)

update :: Context -> VarName -> Int -> Context
update (Context vs) x v = Context $ map (x `addValue` v) vs where
  addValue x' v' (y,vs') = if y == x' then (y,vs' ++ [v']) else (y,vs')

getCurrent :: VarName -> Context -> Int
getCurrent x = last . getAll x

getAll :: VarName -> Context -> [Int]
getAll x (Context vs) = fromJust $ lookup x vs

evalP :: Context -> Predicate -> Bool
evalP d (UIntP p x) = p (getCurrent x d)
evalP d (BIntP p (x,y)) = p (getCurrent x d) (getCurrent y d)
evalP d (UListP p x) = p (getAll x d)
evalP d (BListP p (x,y)) = p (getAll x d) (getAll y d)
evalP d (MixedP p (x,y)) = p (getAll x d) (getCurrent y d)

evalF :: Context -> Function -> Int
evalF d (UIntF f x) = f (getCurrent x d)
evalF d (BIntF f (x,y)) = f (getCurrent x d) (getCurrent y d)
evalF d (UListF f x) = f (getAll x d)
evalF d (BListF f (x,y)) = f (getAll x d) (getAll y d)
evalF d (MixedF f (x,y)) = f (getAll x d) (getCurrent y d)
evalF _ Optional = error "can't evaluate epsilon"

class HasVariables a where
  vars :: a -> [VarName]

instance HasVariables Specification where
  vars = nub . go where
    go (L.ReadInput x _) = [x]
    go (s1 :<> s2) = vars s1 ++ vars s2
    go (L.TillT s) = vars s
    go (L.Branch _ s1 s2) = vars s1 ++ vars s2
    go (L.WriteOutput _) = []
    go L.Nop = []
    go T = []

instance HasVariables Spec where
  vars = nub . go where
    go (Read x _ s) = x : vars s
    go (T.TillT s s') = vars s ++ vars s'
    go (T.Branch _ s1 s2 s3) = vars s1 ++ vars s2 ++ vars s3
    go (Write _ s) = vars s
    go T.Nop = []
    go (InternalT s) = vars s
    go (JumpPoint s s') = vars s ++ vars s'
