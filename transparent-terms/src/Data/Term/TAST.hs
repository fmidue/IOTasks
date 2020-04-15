{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Data.Term.TAST where

import Type.Reflection
import Data.Proxy
import Data.Environment

data TAST v a where
  TLit :: Typeable a => (String,a) -> TAST v a
  TLam :: (Typeable a, Typeable x) => String -> (TAST v x -> TAST v a) -> TAST v (x -> a)
  TApp :: Typeable a => TAST v (a -> b) -> TAST v a -> TAST v b
  TInfix :: (Typeable a, Typeable b) => TAST v a -> TAST v (a -> b -> c) -> TAST v b -> TAST v c
  TCVar :: Typeable a => v -> TAST v a
  TAVar :: Typeable a => v -> TAST v [a]

evalTAST :: (PVarEnv env v, Typeable a, Show v, Show (env v)) => TAST v a -> env v -> a
evalTAST (TLit (_,x)) _ = x
evalTAST (TLam var b) e = \x -> evalTAST (b (TLit (var,x))) e
evalTAST (TApp f a) e = evalTAST f e $ evalTAST a e
evalTAST (TInfix a f b) e = evalTAST f e (evalTAST a e) (evalTAST b e)
evalTAST (TCVar x) e =
  case lookupLastAtType Proxy x e of
    Left err -> error $ printLookupError err
    Right a -> a
evalTAST (TAVar x) e =
  case lookupAllAtType Proxy x e of
    Left err -> error $ printLookupError err
    Right a -> a

printTAST :: (v -> String) -> TAST v a -> String
printTAST _ (TLit (x,_)) = x
printTAST sho (TLam x t) = "\\" ++ x ++ " -> " ++ printTAST sho (t $TLit (x,undefined))
printTAST sho (TApp f x) = "(" ++ printTAST sho f ++ ") ("++ printTAST sho x ++ ")"
printTAST sho (TInfix x f y) = "(" ++ printTAST sho x ++ ") " ++ printTAST sho f ++ " ("++ printTAST sho y ++ ")"
printTAST sho (TCVar x) = sho x ++ "_C"
printTAST sho (TAVar x) = sho x ++ "_A"
