{-# LANGUAGE ViewPatterns #-}
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

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard)

import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List (nub,find)

import Data.Term.AST
import Data.Environment

import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.FreshVar

data Rewrite m = Rewrite
  { rewrite :: IRProgram -> Maybe (IRProgram -> m IRProgram)
  , desc :: String
  }

applyRewrite :: Applicative m => Rewrite m -> IRProgram -> m (Maybe IRProgram)
applyRewrite r x = traverse ($ x) (rewrite r x)

isApplicable :: Rewrite m -> IRProgram -> Bool
isApplicable r x = isJust $ rewrite r x

-- interactive rewriting
globalRewrites :: [Rewrite FreshVarM]
globalRewrites =
  [ inlineRewrite ]

loopRewrites :: F -> [Rewrite FreshVarM]
loopRewrites (f,ps,_is) =
  accumRewrite f : [ foldRewrite f i | i <- [0..length ps -1] ]

rewriteOptions :: IRProgram -> [Rewrite FreshVarM]
rewriteOptions ir@(_,_,fs) =
  filter (`isApplicable` ir) $ globalRewrites ++ concatMap loopRewrites fs

rewriteRepl :: IRProgram -> [(Varname, Int)] -> IO IRProgram
rewriteRepl p env = do
  putStrLn "Current program:"
  print $ printIRProgram p
  let opts = rewriteOptions p
      n = length opts
  if n > 0
    then do putStrLn $ "Choose rewrite (1-" ++ show n ++ ") or finish (q)"
            putStrLn . unlines $ zipWith (\i d -> i ++ ": " ++ d) (map show [1..n]) (map desc opts)
    else putStrLn "No rewrite options, press q to exit"
  inp <- getLine
  if inp == "q"
    then return p
    else
      let (Just p',env') = runFreshVarM  (applyRewrite (opts !! (read @Int inp - 1)) p) env
      in rewriteRepl p' env'

-- optimizing IRProgram
optimize :: IRProgram -> IRProgram
optimize (i,d,f) = (i,d',f)
  where d' = inlineAll d

inlineRewrite :: Applicative m => Rewrite m
inlineRewrite = Rewrite t "inline intermediate definitions" where
    t (_,ds,_) =
      -- try if there is at least one inline possibility
      if any (isJust . flip inline1 ds) ds
        then Just $ \(is,ds,fs) -> pure (is,inlineAll ds,fs)
        else Nothing

inlineAll :: [Def] -> [Def]
inlineAll ds = foldr (\d ds' -> fromMaybe ds'$ inline1 d ds') ds ds

inline :: (Var,DefRhs) -> DefRhs -> Maybe DefRhs
inline (x,t) (U f y v)
  | x == y = Just $ N f t v
  | otherwise = Nothing
inline (x,t) (N f y v) = do
  y' <- inline (x,t) y
  return $ N f y' v
inline _ (Const _) = Nothing

inline1 :: Def -> [Def] -> Maybe [Def]
inline1 (x,t,1,_) ds =
      let ds' = sequenceA [ f (x,t) d <|> Just d | d@(y,_,_,_) <- ds, y /= x ]
          changed = any isJust $ map (f (x,t)) ds -- duplicate computation TODO: fix
      in if changed
        then ds' --guaranteed to be Just
        else Nothing
  where
    f x (y,d,n,scp) = do
      rhs <-inline x d
      pure (y,rhs,n,scp)
inline1 _ _ = Nothing

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
      Just (Const tRhs,n,_) | n <= 1 && vars tRhs == [rv] ->
        let
          r y = mapV (\x -> if x == rv then y else x) tRhs
          (_,b) = fromJust $ lookupF f fs
          (b',ds',fs') = transformProgram tr (b,ds,fs)
          tr ds fs = (idFold ds fs){ fYield = trYield ds fs }
          trYield ds fs [rv'] = ([PRINT (inc t 100)], (inc t 100,Const (r rv'),1,_) : ds ,fs)
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
transformProgram _ ([],ds,fs) = ([],ds,fs)
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

foldRewrite :: Var -> Int ->  Rewrite FreshVarM
foldRewrite f i = Rewrite t $ "inline 'external' accumulation for paramter " ++ show i ++ " of " ++ name f
  where
    t (is,ds,fs) =
      -- this condition is quite conservative right now to avoid complexity
      let bCs = bindCalls is
          [(_,rvs)] = bCs
          -- ASSUMPTION: BINDCALL as generated from specifications have the same number of parameters and return variables
          -- this rules out application of this transformation on newly introduced accumulators
          isBaseParam = i < length rvs
          -- check how many different functions are used on the accumulation parameters inside the function
          -- if its just one (++) than we can apply the transformation
          notUsedOtherwise =  maybe False ((== ["++"]). nub . map printFlat . (\x -> usedOnBaseInScope (baseName x) (name f) ds) . (!! i) . fst) $ lookupF f fs
          uc = fromMaybe 0 $ lookup (rvs !! i) $ usedVars ds
      in if length bCs == 1 && isBaseParam && notUsedOtherwise && uc == 1 && isJust (lookupF f fs)
        then Just $ foldOpt f i
        else Nothing
    bindCalls = concatMap @[] (\case BINDCALL g ps rvs -> [(ps,rvs) | f == g]; _ -> [])

-- TODO: generalize to all parameters
-- "inlining" fold optimization
foldOpt :: Var -> Int -> IRProgram -> FreshVarM IRProgram
foldOpt _ _ ([],ds,fs) = return ([],ds,fs)
-- again, this pattern is conservative to reduce complexity
foldOpt f n (BINDCALL g (elemAt n -> (ps1,p,ps2)) rvs : PRINT t : is,ds,fs)
  | f == g =
  case lookupDef t ds of
    Just (rhs,1,_) ->
      case extractAlgebra (toAST rhs) (rvs !! n) p of
        Just (g, x) -> do
          r <- toFoldAccum g f n fs ds
          (is',ds',fs') <- foldOpt f n (is,
                           updateDef (t,Const x) r,
                           fs)
          return (BINDCALL f (ps1++t:ps2) rvs : PRINT (rvs !! n) : is', ds',fs')
        _ -> do
          (is',ds',fs') <- foldOpt f n (is,ds,fs)
          return (BINDCALL f [p] rvs : PRINT t : is',ds',fs')
    _ -> error "no definition found"
  | otherwise = do
    (is',ds',fs') <- foldOpt f n (is,ds,fs)
    return (BINDCALL g (ps1++p:ps2) rvs : PRINT t : is',ds',fs')
foldOpt f n (i:is,ds,fs) = do
  (is',ds',fs') <- foldOpt f n (is,ds,fs)
  return (i:is',ds',fs')

elemAt :: Int -> [a] -> ([a],a,[a])
elemAt n xs
  | length xs > n =
    let (as,b:bs) = splitAt n xs
    in (as,b,bs)
  | otherwise = error "elemAt: index to large"

data ChangeMode = Add Varname | Replace

-- NOTE: This is only sound if the accumulation parameter is not printed out!
toFoldAccum :: (AST Var -> AST Var -> AST Var) -> Var -> Int -> [F] -> [Def] -> FreshVarM [Def]
toFoldAccum g f i fs ds =
  let
    (ps,b) = fromJust $ lookupF f fs
    xs = filterJust (inputDependency ps ds) (==(ps !! i)) $ leavingVars b
  in foldM (\ds' x -> maybe ds' fst <$> changeUpdate Replace x g ds') ds xs

filterJust :: (a -> Maybe b) -> (b -> Bool) -> [a] -> [a]
filterJust f p = filter (maybe False p . f)

inputDependency :: [Var] -> [Def] -> Var -> Maybe Var
inputDependency ps ds x = find (== rootVar ds x) ps

rootVar :: [Def] -> Var -> Var
rootVar ds x = fromMaybe x $ do
  (rhs,_,_) <- lookupDef x ds
  y <- baseVar rhs
  return $ rootVar ds y

changeUpdate :: ChangeMode -> Var -> (AST Var -> AST Var -> AST Var) -> [Def] -> FreshVarM (Maybe ([Def],Var))
changeUpdate m x g ds = case break (\(z,_,_,_) -> x == z) ds of
  (d1, (x,rhs,n,scp):d2) -> do
    (d1',d2') <- case baseVar rhs of
      Just y -> do
        d1' <- maybe d1 fst <$> changeUpdate m y g d1
        d2' <- maybe d2 fst <$> changeUpdate m y g d2
        return (d1',d2')
      Nothing -> return (d1,d2)
    case m of
      Replace -> return $ Just (d1' ++ (x,replaceUpdate g rhs,n,scp) : d2',x)
      Add base -> do
        accC <- currentName base
        accF <- freshName base
        return $ Just (d1' ++ [(x,rhs,n,scp),(accF,replaceUpdate g (changeBaseVar accC rhs),n,scp)] ++ d2',accF)
  _ -> return Nothing

replaceUpdate :: (AST Var -> AST Var -> AST Var) -> DefRhs -> DefRhs
replaceUpdate f (U _ y v) = U f y v
replaceUpdate f (N _ y v) = N f (replaceUpdate f y) v
replaceUpdate _ (Const t) = Const t

-- the base variable which this rhs updates or Nothing if it is a constant
baseVar :: DefRhs -> Maybe Var
baseVar (U _ y _) = Just y
baseVar (N _ y _) = baseVar y
baseVar (Const _) = Nothing

changeBaseVar :: Var -> DefRhs -> DefRhs
changeBaseVar x (U f _ v) = U f x v
changeBaseVar x (N f y v) = N f (changeBaseVar x y) v
changeBaseVar _ (Const t) = Const t

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
extractAlgebra (App (Leaf f) (Var x)) rv p
  | x == rv = do
    (g,z) <- lookup f knownFolds
    pure (g,z p)
extractAlgebra _ _ _ = Nothing

initialAccum :: String -> String -> Var -> AST Var
initialAccum _ i (Initial _) = Leaf i
initialAccum f _ p = App (Leaf f) (Leaf $ name p)

knownFolds :: [(String,(AST Var -> AST Var -> AST Var, Var -> AST Var))]
knownFolds =
  [ ("sum",(\b a -> App (PostApp b (Leaf "+")) a, initialAccum "sum" "0"))
  , ("length",(\b _ -> App (PostApp b (Leaf "+")) (Leaf "1"), initialAccum "length" "0"))
  ]

accumRewrite :: Var -> Rewrite FreshVarM
accumRewrite f = Rewrite t $ "introduce auxillary accumulation parameter(s) for " ++ name f where
  t (_,ds,fs) = do
    (ps,_) <- lookupF f fs
    let usedOtherwise = any (any (`elem` map fst knownFolds)) $ map printFlat . (\x -> usedOnBaseInScope (baseName x) (name f) ds) <$> ps
    guard usedOtherwise
    pure accumOpt

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
        newVs = map (\(x,_,_,_) -> x) vds
    in \(is,ds) -> (BINDCALL g (if f == g then ps ++ newVs else ps) rvs : is, vds ++ ds)
  x -> \(is,ds) -> (x:is,ds)) ([],[])

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
          let [os] = nub $ filter (\x -> baseName x == baseName p && x /= p) $ leavingVars is
          changeUpdate (Add "acc") os g ds'
            >>= \case
              Just (ds',xNew) -> do
                xp <- freshName "acc"
                return (changeUsage f p x1 ds',xs++[Just $ \x -> (xp,Const (v x),0,"main")],ys++[x1],addNewParameter xNew fVar is')
              Nothing -> return (ds',xs++[Nothing],ys,is')
        Nothing -> return (ds',xs++[Nothing],ys,is')
    ) (ds,[],[],is) (map printFlat $ usedOn p ds)

foldParameters :: String -> Maybe (AST Var -> AST Var -> AST Var, Var -> AST Var)
foldParameters "length" = Just (\y _ -> App (PostApp y (Leaf "+")) (Leaf "1"), initialAccum "length" "0")
foldParameters _ = Nothing

-- first parameter are the basenames of the new accumulation parameter and the old one it is derived from
addNewParameter :: Var -> Var -> [Instruction] -> [Instruction]
addNewParameter new f = map $ \case
  READ x -> READ x
  PRINT x -> PRINT x
  IF c t e -> IF c (addNewParameter new f t) (addNewParameter new f e)
  TAILCALL g ps ->
    if f == g
      then TAILCALL f (ps ++ [new])
      else TAILCALL g ps
  BINDCALL g ps rvs -> BINDCALL g ps rvs
  YIELD x -> YIELD x
  NOP -> NOP

addParameter :: Var -> Var -> [F] -> [F]
addParameter f p = updateF f $ \(f,ps,b) -> (f,ps ++ [p],b)

usedOnBaseInScope :: Varname -> Varname -> [Def] -> [AST Var]
usedOnBaseInScope x scp ds = flip usedOn ds =<< filter ((== x) . baseName) (allVarsInScope scp ds)

usedOn :: Var -> [Def] -> [AST Var]
usedOn _ [] = []
usedOn x ((_,U f y v,_,_):ds) = usedOn' x (f (Var y) (Var v)) ++ usedOn x ds
usedOn x ((_,N f y v,_,_):ds) = usedOn' x (f (toAST y) (Var v)) ++ usedOn x ds
usedOn x ((_,Const t,_,_):ds) = usedOn' x t ++ usedOn x ds

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
  (y,Const t,n,scp) -> [(y,Const $ replaceFunctionUsage (f,x) p t,n,scp)]
  (y,d,n,scp) -> [(y,d,n,scp)]
