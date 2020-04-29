{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Data.Maybe (fromMaybe, fromJust)

import Data.Term (termVars)
import Data.Term.Typed.AST
import Data.Term.Liftable (litT)
import qualified Data.Term.Liftable.Prelude as T
import Data.Environment

import Data.Proxy
import Type.Reflection (Typeable,(:~~:)(..), typeRep, eqTypeRep)

import Test.IOTasks.CodeGeneration.IR

-- optimizing IRProgram
optimize :: IRProgram -> IRProgram
optimize (i,d,f) = (i,d',f)
  where d' = simplify (emptyEnvironment @Environment) d

simplify :: (VarEnv env Var, Typeable a) => env Var -> [Def a] -> [Def a]
simplify e ds = case inline1 ds of
  Just ds' -> simplify e ds'
  Nothing -> map (\(x,d,n) -> (x,reduceD e d,n)) ds

reduceD :: VarEnv env Var => env Var -> DefRhs a -> DefRhs a
reduceD e (Forget x) = Forget (reduceD e x)
reduceD e (U f y v) = U (\y v  -> reduceT e $ f y v) y v
reduceD e (N1L f y v) = N1L (\y v  -> reduceT e $ f y v) y v
reduceD e (N1R f y v) = N1R (\y v  -> reduceT e $ f y v) y v
reduceD e (N2 f y v) = N2 (\y v  -> reduceT e $ f y v) y v
reduceD e (Const t) = Const (reduceT e t)

inline :: (Typeable x, Typeable a) => (Var,DefRhs x) -> DefRhs a -> Maybe (DefRhs a)
inline (x,t :: DefRhs x) (Forget y) = Forget <$> inline (x,t) y
inline (x,t :: DefRhs x) (U (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == y = case eqTypeRep (typeRep @x) (typeRep @a) of
    Just HRefl -> Just $ N1L f t v
    Nothing -> Nothing
  | x == v = case eqTypeRep (typeRep @x) (typeRep @b) of
    Just HRefl -> Just $ N1R f y t
    Nothing -> Nothing
  | otherwise = Nothing
inline (x,t :: DefRhs x) (N1L (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == v = case eqTypeRep (typeRep @x) (typeRep @b) of
    Just HRefl -> Just $ N2 f y t
    Nothing -> Nothing
  | otherwise = (\y' -> N1L f y' v) <$> inline (x,t) y
inline (x,t :: DefRhs x) (N1R (f :: AST Var a -> AST Var b -> AST Var c) y v)
  | x == y = case eqTypeRep (typeRep @x) (typeRep @a) of
    Just HRefl -> Just $ N2 f t v
    Nothing -> Nothing
  | otherwise = N1R f y <$> inline (x,t) v
inline (x,t) (N2 f y v)
  = case (inline (x,t) y, inline (x,t) v) of
    (Nothing, Nothing) -> Nothing
    (Just y', Nothing) -> Just $ N2 f y' v
    (Nothing, Just v') -> Just $ N2 f y v'
    -- this probably does not happen, at least not
    -- if only variables with 1 occurence are inlined
    (Just y', Just v') -> Just $ N2 f y' v'
inline _ (Const t) = Nothing

inline1 :: Typeable a => [Def a] -> Maybe [Def a]
inline1 ds =
  case break (\(_,_,n) -> n == 1) ds of
    (_,[]) -> Nothing
    (xs,(x,Forget t,_):ys) -> Just $ foldr (f (x,t)) [] (xs++ys)
    (xs,(x,t,_):ys) -> Just $ foldr (f (x,t)) [] (xs++ys)
  where
    f x (y,d,n) ds' = case inline x d of
      Nothing -> (y,d,n) : ds'
      Just d' -> (y,d',n) : ds'

-- further optimizations

-- NOTE: there is a naming issue here if we do this for a loop with more then one
-- retrun point (or a program that uses the name t', which should not happen, because of IndexedVar)
inlinePrint :: IRProgram -> IRProgram
inlinePrint ([],ds,fs) = ([],ds,fs)
inlinePrint p@(BINDCALL f ps [rv] : PRINT t : is,ds,fs)
  | rv == t =
    let
      (_,b) = fromJust $ lookupF f fs
      (b',ds',fs') = transformProgram tr (b,ds,fs)
      tr ds fs = (idFold ds fs){ fYield = trYield ds fs }
      trYield ds fs [rv'] = ([PRINT rv'], ds ,fs)
      trYield ds fs rvs = ([YIELD rvs], ds, fs)
    in (BINDCALL f ps [] : is,ds',updateF f (const b') fs')
  | otherwise =
    case lookupDef t ds of
      Just (Forget (Const tRhs),n) | n <= 1 && termVars tRhs == [rv] ->
        let
          r y = mapV (\x -> if x == rv then y else x) tRhs
          (_,b) = fromJust $ lookupF f fs
          (b',ds',fs') = transformProgram tr (b,ds,fs)
          tr ds fs = (idFold ds fs){ fYield = trYield ds fs }
          trYield ds fs [rv'] = ([PRINT (t++"'")], (t++"'",Forget $ Const (r rv'),1) : ds ,fs)
          trYield ds fs rvs = ([YIELD rvs], ds, fs)
        in (BINDCALL f ps [] : is,ds',updateF f (const b') fs')
      _ -> p
inlinePrint (i:is,ds,fs) = let (is',ds',fs') = inlinePrint (is,ds,fs) in (i:is',ds',fs')

data InstFold a = InstFold
  { fRead :: Var -> a
  , fPrint :: Var -> a
  , fIf :: Var -> a -> a -> a
  , fTailCall :: Var -> [Var] -> a
  , fBindCall :: Var -> [Var] -> [Var] -> a
  , fYield :: [Var] -> a
  , fNop :: a
  }

idFold :: [Def ()] -> [F] -> InstFold IRProgram
idFold ds fs =
  let
    fIf c (t,_,_) (e,ds',fs') = ([IF c t e], ds', fs')
    fRead x = ([READ x], ds, fs)
    fPrint x = ([PRINT x], ds, fs)
    fTailCall x y = ([TAILCALL x y], ds, fs)
    fBindCall x y z = ([BINDCALL x y z], ds, fs)
    fYield x = ([YIELD x], ds, fs)
    fNop = ([NOP], ds, fs)
  in InstFold{..}

transformProgram :: ([Def ()] -> [F] -> InstFold IRProgram) -> IRProgram -> IRProgram
transformProgram f ([],ds,fs) = ([],ds,fs)
transformProgram f (i:is,ds,fs) =
  let
    InstFold{..} = f ds fs
    (is',ds',fs') = case i of
      READ x -> fRead x
      PRINT x -> fPrint x
      IF c t e ->
        let
          (t',ds',fs') = transformProgram f (t,ds,fs)
          (e',ds'',fs'') = transformProgram f (e,ds',fs')
        in fIf c (t',ds'',fs'') (e',ds'',fs'')
      TAILCALL f ps -> fTailCall f ps
      BINDCALL f ps rvs -> fBindCall f ps rvs
      YIELD ps -> fYield ps
      NOP -> fNop
    (is'',ds'',fs'') = transformProgram f (is,ds',fs')
  in (is' ++ is'', ds'', fs'')

-- "inlining" fold optimization
foldOpt :: IRProgram -> IRProgram
foldOpt ([],ds,fs) = ([],ds,fs)
foldOpt (BINDCALL f [p] [rv] : PRINT t : is,ds,fs) =
  case lookupDef t ds of
    Just (Forget (rhs :: DefRhs a),0) ->
      case extractAlgebra (toAST rhs) rv p of
        Just (Algebra g x) ->
          let (is',ds',fs') = foldOpt (is,updateDef (t,Forget $ Const x) $ toFoldAccum g f fs ds, fs)
          in (BINDCALL f [t] [rv] : PRINT rv : is', ds',fs')
        _ -> let (is',ds',fs') = foldOpt (is,ds,fs) in (BINDCALL f [p] [rv] : PRINT t : is',ds',fs')
    _ -> error "no definition found"
foldOpt (i:is,ds,fs) = let (is',ds',fs') = foldOpt (is,ds,fs) in (i:is',ds',fs')

-- NOTE: This is only sound if the accumulation parameter is not printed out!
toFoldAccum :: (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var b) -> Var -> [F] -> [Def ()] -> [Def ()]
toFoldAccum g f fs ds =
  let
    (_,b) = fromJust $ lookupF f fs
    xs = leavingVars b
  in foldr (\x ds' -> fromMaybe ds' $ changeUpdate x g ds') ds xs

changeUpdate :: (Typeable a, Typeable b) => Var -> (AST Var a -> AST Var b -> AST Var b) -> [Def ()] -> Maybe [Def ()]
changeUpdate x g ds = case break (\(z,_,_) -> x == z) ds of
  (d1, (x,Forget (U _ y v),n):d2) ->
    case changeUpdate y g $ d1 ++ (x,Forget $ U g y v,n) : d2 of
      Just ds' -> Just ds'
      Nothing -> Just $ d1 ++ (x,Forget $ U g y v,n) : d2
  _ -> Nothing

leavingVars :: [Instruction] -> [Var]
leavingVars = concatMap f where
  f :: Instruction -> [Var]
  f (READ _) = []
  f (PRINT _) = []
  f (IF _ t e) = leavingVars t ++ leavingVars e
  f (TAILCALL _ ps) = ps
  f (BINDCALL _ ps _) = ps -- note that BINDCALL normaly should not appear inside a body
  f (YIELD ps) = ps
  f NOP = []

-- extractAlgebra t tries to extracts the algebra from an AST
-- i.e. the parameters for expressing the represented term as a fold
-- incomplete implementation. Currently not sure how to write this in a general way.
-- especially the starting value is not correct in a general setting.
extractAlgebra :: Typeable a => AST Var a -> Var -> Var -> Maybe (Algebra a)
extractAlgebra (App (Leaf (f :: x -> a) name) (Var x _)) rv p | x == rv =
  let
    initialV :: Show a => a -> AST Var a
    initialV v = if p == "[]" then Leaf v (show v) else initialAccum f name p
  in case typeRep @a `eqTypeRep` typeRep @Int of
    Just HRefl -> case name of
      -- folds to Int
      "sum" -> Just $ Algebra (\b a -> b T.+ coerseAST a Proxy) (initialV 0)
      "length" -> Just $ Algebra (\b _ -> b T.+ litT 1) (initialV 0)
      _ -> Nothing
    Nothing -> Nothing
extractAlgebra _ _ _ = Nothing

initialAccum :: Typeable x => (x -> a) -> String -> Var -> AST Var a
initialAccum f name p = App (Leaf f name) (Var p typeRep)

data Algebra b = Algebra { c :: forall a. Typeable a => AST Var b -> AST Var a -> AST Var b, n :: AST Var b }

coerseAST :: (Typeable a, Typeable b) => AST Var a -> Proxy b -> AST Var b
coerseAST (x :: AST Var a) (Proxy :: Proxy b) =
  case typeRep @a `eqTypeRep` typeRep @b of
    Just HRefl -> x
    Nothing -> error $ "coersion failed from " ++ show (typeRep @a) ++ " to " ++ show (typeRep @b)
