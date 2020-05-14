{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Control.Monad (foldM)

import Data.Maybe (fromMaybe, fromJust)
import Data.List (nub)

import Data.Term.AST
import Data.Environment

import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.FreshVar

import Debug.Trace

-- optimizing IRProgram
optimize :: IRProgram -> IRProgram
optimize (i,d,f) = (i,d',f)
  where d' = simplify (emptyEnvironment @Environment) d

simplify :: VarEnv env Var => env Var -> [Def] -> [Def]
simplify e ds = case inline1 ds of
  Just ds' -> simplify e ds'
  Nothing -> map (\(x,d,n) -> (x,d,n)) ds

inline :: (Var,DefRhs) -> DefRhs -> Maybe DefRhs
inline (x,t) (U f y v)
  | x == y = Just $ N1L f t v
  | x == v = Just $ N1R f y t
  | otherwise = Nothing
inline (x,t) (N1L f y v)
  | x == v = Just $ N2 f y t
  | otherwise = (\y' -> N1L f y' v) <$> inline (x,t) y
inline (x,t) (N1R f y v)
  | x == y = Just $ N2 f t v
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

inline1 :: [Def] -> Maybe [Def]
inline1 ds =
  case break (\(_,_,n) -> n == 1) ds of
    (_,[]) -> Nothing
    (xs,(x,t,_):ys) -> Just $ foldr (f (x,t)) [] (xs++ys)
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
    in (BINDCALL f ps [] : is,ds',updateF f (\(f,p,_) -> (f,p,b')) fs')
  | otherwise =
    case lookupDef t ds of
      Just (Const tRhs,n) | n <= 1 && vars tRhs == [rv] ->
        let
          r y = mapV (\x -> if x == rv then y else x) tRhs
          (_,b) = fromJust $ lookupF f fs
          (b',ds',fs') = transformProgram tr (b,ds,fs)
          tr ds fs = (idFold ds fs){ fYield = trYield ds fs }
          trYield ds fs [rv'] = ([PRINT (inc t 100)], (inc t 100,Const (r rv'),1) : ds ,fs)
          trYield ds fs rvs = ([YIELD rvs], ds, fs)
        in (BINDCALL f ps [] : is,ds',updateF f (\(f,p,_) -> (f,p,b')) fs')
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

idFold :: [Def] -> [F] -> InstFold IRProgram
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

transformProgram :: ([Def] -> [F] -> InstFold IRProgram) -> IRProgram -> IRProgram
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

-- TODO: generalize to all parameters
-- "inlining" fold optimization
foldOpt :: IRProgram -> IRProgram
foldOpt ([],ds,fs) = ([],ds,fs)
foldOpt (BINDCALL f (p:ps) [rv] : PRINT t : is,ds,fs) =
  case lookupDef t ds of
    Just (rhs,0) ->
      case extractAlgebra (toAST rhs) rv p of
        Just (g, x) ->
          let (is',ds',fs') = foldOpt (is,
                              updateDef (t,Const x) $ toFoldAccum g f fs ds,
                              fs)
          in (BINDCALL f (t:ps) [rv] : PRINT rv : is', ds',fs')
        _ -> let (is',ds',fs') = foldOpt (is,ds,fs) in (BINDCALL f [p] [rv] : PRINT t : is',ds',fs')
    _ -> error "no definition found"
foldOpt (i:is,ds,fs) = let (is',ds',fs') = foldOpt (is,ds,fs) in (i:is',ds',fs')

data ChangeMode = Add Var | Replace

-- NOTE: This is only sound if the accumulation parameter is not printed out!
toFoldAccum :: (AST Var -> AST Var -> AST Var) -> Var -> [F] -> [Def] -> [Def]
toFoldAccum g f fs ds =
  let
    (_,b) = fromJust $ lookupF f fs
    xs = leavingVars b
  in foldr (\x ds' -> fromMaybe ds' $ changeUpdate Replace x g ds') ds xs

changeUpdate :: ChangeMode -> Var -> (AST Var -> AST Var -> AST Var) -> [Def] -> Maybe [Def]
changeUpdate m x g ds = case break (\(z,_,_) -> x == z) ds of
  (d1, (x,U f y v,n):d2) ->
    case m of
      Replace ->
        case changeUpdate m y g $ d1 ++ (x,U g y v,n) : d2 of
          Just ds' -> Just ds'
          Nothing -> Just $ d1 ++ (x,U g y v,n) : d2
      -- TODO: This is not quite correct!
      Add p -> case changeUpdate m y g $ d1 ++ (p,U g (changeBaseName y (baseName p)) v,n) : (x,U f y v,n) : d2 of
        Just ds' -> Just ds'
        Nothing -> Just $ d1 ++ (p,U g (changeBaseName y (baseName p)) v,n) : (x,U f y v,n) : d2
  (_, (_,N1L{},_):_) -> error "not implemented changeUpdate for N1L"
  (_, (_,N1R{},_):_) -> error "not implemented changeUpdate for N1R"
  (_, (_,N2{},_):_) -> error "not implemented changeUpdate for N2"
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
extractAlgebra :: AST Var -> Var -> Var -> Maybe (AST Var -> AST Var -> AST Var, AST Var)
extractAlgebra (App (Leaf f) (Var x)) rv p | x == rv =
  case f of
    -- folds to Int
    "sum" -> Just (\b a -> App (PostApp b (Leaf "+")) a, initialAccum f "0" p)
    "length" -> Just (\b _ -> App (PostApp b (Leaf "+")) (Leaf "1"), initialAccum f "0" p)
    _ -> Nothing
extractAlgebra _ _ _ = Nothing

initialAccum :: String -> String -> Var -> AST Var
initialAccum _ i (Initial _) = Leaf i
initialAccum f i p = App (Leaf f) (Leaf $ name p)

-- change usage of a variable expressable by "folding accumulation" into an additional loop parameter
accumOpt :: IRProgram -> FreshVarM IRProgram
accumOpt (is,ds,fs) =
  foldM (\(is',ds',fs') f -> do
    (is'',ds'',f') <- introAccums is' ds' f
    return (is'',ds'',fs'++[f'])
    ) (is,ds,[]) fs

addArguments :: Var -> [Maybe (Var -> Def)] -> [Instruction] -> ([Instruction],[Def])
addArguments f p = foldr (\case
  BINDCALL g ps rvs ->
    let vds = concat $ zipWith (\g x -> maybe [] pure (g <*> pure x)) p ps
        newVs = map (\(x,_,_) -> x) vds
    in \(is,ds) -> (BINDCALL g (if f == g then ps ++ newVs else ps) rvs : is, vds ++ ds)
  x -> \(is,ds) -> (x:is,ds)) ([],[])

-- TODO: generalize to more than one parameter
introAccums :: [Instruction] -> [Def] -> F -> FreshVarM ([Instruction],[Def],F)
introAccums gis ds (f,ps,fis) = do
  (gis',ds',(ps',fis')) <- foldM (\(gis',ds',(ps',fis')) p -> do
      (ds',ivs,ys,fis') <- introSingleAccum ds' f p fis'
      let (is,ds'') = addArguments f ivs gis'
      return (is, ds' ++ ds'',(ps' ++ ys ,fis'))
    ) (gis,ds,(ps,fis)) ps
  return (gis',ds',(f,ps',fis'))

introSingleAccum :: [Def] -> Var -> Var -> [Instruction] -> FreshVarM ([Def],[Maybe (Var -> Def)],[Var],[Instruction])
introSingleAccum ds fVar p is =
    foldM (\(ds',xs,ys,is') f ->
      case foldParameters f of
        Just (g,v) -> do
          x1 <- freshName "acc"
          x2 <- freshName "acc"
          xp <- freshName "acc"
          let [os] = nub $ filter (\x -> baseName x == baseName p && x /= p) $ leavingVars is
          case changeUpdate (Add x2) os g ds' of
            Just ds' -> return (changeUsage f p x1 ds',xs++[Just $ \x -> (xp,Const (v x),0)],ys++[x1],addNewParameter (baseName x1,baseName p) fVar is')
            Nothing -> return (ds',xs++[Nothing],ys,is')
        Nothing -> return (ds',xs++[Nothing],ys,is')
    ) (ds,[],[],is) (map printFlat $ usedOn p ds)

foldParameters :: String -> Maybe (AST Var -> AST Var -> AST Var, Var -> AST Var)
foldParameters "length" = Just (\y _ -> App (PostApp y (Leaf "+")) (Leaf "1"), initialAccum "length" "0")
foldParameters _ = Nothing

-- first parameter are the basenames of the new accumulation parameter and the old one it is derived from
addNewParameter :: (String,String) -> Var -> [Instruction] -> [Instruction]
addNewParameter (new,old) f = map $ \case
  READ x -> READ x
  PRINT x -> PRINT x
  IF c t e -> IF c (addNewParameter (new,old) f t) (addNewParameter (new,old) f e)
  TAILCALL g ps ->
    if f == g
      then
        let [Indexed _ i] = filter (\x -> old == baseName x) ps -- ASSUMPTION: tailcalls dont have two parameters with the same baseName
        in TAILCALL f (ps ++ [Indexed new i])
      else TAILCALL g ps
  BINDCALL g ps rvs -> BINDCALL g ps rvs
  YIELD x -> YIELD x
  NOP -> NOP

addParameter :: Var -> Var -> [F] -> [F]
addParameter f p = updateF f $ \(f,ps,b) -> (f,ps ++ [p],b)

usedOn :: Var -> [Def] -> [AST Var]
usedOn _ [] = []
usedOn x ((_,U f y v,_):ds) = usedOn' x (f (Var y) (Var v)) ++ usedOn x ds
usedOn x ((_,N1L f y v,_):ds) = usedOn' x (f (toAST y) (Var v)) ++ usedOn x ds
usedOn x ((_,N1R f y v,_):ds) = usedOn' x (f (Var y) (toAST v)) ++ usedOn x ds
usedOn x ((_,N2 f y v,_):ds) = usedOn' x (f (toAST y) (toAST v)) ++ usedOn x ds
usedOn x ((_,Const t,_):ds) = usedOn' x t ++ usedOn x ds

usedOn' :: Var -> AST Var -> [AST Var]
usedOn' _ (Leaf _) = []
usedOn' x (Lam _ t) = usedOn' x $ t ""
usedOn' _ (Var _) = []
usedOn' _ (VarA _) = []
usedOn' x (App f (Var y)) | x == y = f : usedOn' x f
usedOn' x (App f y) = usedOn' x f ++ usedOn' x y
usedOn' x (PostApp (Var y) f) | x == y = f : usedOn' x f
usedOn' x (PostApp y f) = usedOn' x y ++ usedOn' x f

replaceFunctionUsage :: (String,Var) -> Var -> AST Var -> AST Var
replaceFunctionUsage _ _ (Leaf s) = Leaf s
replaceFunctionUsage (f,x) p (Lam y t) = Lam y (replaceFunctionUsage (f,x) p . t)
replaceFunctionUsage _ _ (Var x) = Var x
replaceFunctionUsage _ _ (VarA x) = VarA x
replaceFunctionUsage (f,x) p (App (Leaf g) (Var y)) | (f,x) == (g,y) = Var p
replaceFunctionUsage (f,x) p (App g y) = App (replaceFunctionUsage (f,x) p g) (replaceFunctionUsage (f,x) p y)
replaceFunctionUsage (f,x) p (PostApp y g) = PostApp (replaceFunctionUsage (f,x) p y) (replaceFunctionUsage (f,x) p g)

changeUsage :: String -> Var -> Var -> [Def] -> [Def]
changeUsage f x p ds = flip concatMap ds $ \case
  (y,Const t,n) -> [(y,Const $ replaceFunctionUsage (f,x) p t,n)]
  (y,d,n) -> [(y,d,n)]
