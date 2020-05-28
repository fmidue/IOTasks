{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Control.Applicative ((<|>), Alternative)
import Control.Monad (foldM, guard)

import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.List (nub,find)

import Data.Term.AST
import Data.Environment

import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.FreshVar

-- general infrastructure
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
loopRewrites f@(fVar,(as,_),_) =
     accumRewrite fVar
  :  [ foldRewrite f i | i <- [0..length as -1] ]
  ++ [ printRewrite f i | i <- [0..length as -1] ]

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

inlineRewrite :: Applicative m => Rewrite m
inlineRewrite = Rewrite t "inline intermediate definitions" where
    t (_,ds,_) =
      -- try if there is at least one inline possibility
      if any (isJust . flip inline1 ds) ds
        then Just $ \(is,ds,fs) -> pure (is,inlineAll ds,fs)
        else Nothing

-- actual rewrites
inlineAll :: [Def] -> [Def]
inlineAll ds = foldr (\d ds' -> fromMaybe ds'$ inline1 d ds') ds ds

inline :: (Var,DefRhs) -> DefRhs -> Maybe DefRhs
inline (x,t) (U f y v) = do
  guard (x == y)
  pure $ N f t v
inline (x,t) (N f y v) = do
  y' <- inline (x,t) y
  pure $ N f y' v
inline _ (Const _) = Nothing

inline1 :: Def -> [Def] -> Maybe [Def]
inline1 (x,t,1,_) ds = do
  ds' <- applyUpdate (update (x,t)) $ filter (\(y,_,_,_) -> y /= x) ds
  guard $ any snd ds' -- check for change
  pure $ map fst ds'
  where
    update x (y,d,n,scp) = do
      rhs <-inline x d
      pure (y,rhs,n,scp)
inline1 _ _ = Nothing

-- apply an update function to each element choosing the new value if it
-- "succeedes" else keeping the old value. Indicates which elements were updated.
applyUpdate :: Alternative f => (a -> f a) -> [a] -> f [(a,Bool)]
applyUpdate update = traverse (\old -> ((,True) <$> update old) <|> pure (old,False))

printRewrite :: Applicative f => F -> Int -> Rewrite f
printRewrite (fVar,(as,_),_) i = Rewrite t $ "move print inside " ++ show fVar where
  t (is,_,_) =
    let
      bCs = bindCalls fVar is
      boundVars = map ((!! i) . snd) bCs
      printed = printVars is
      allPrinted = all (`elem` printed) boundVars
      printedBound = filter (`elem` boundVars) printed
      allPrintedOnce = allPrinted && printedBound == nub printedBound
      isBaseParam = i < length as
    in if isBaseParam && allPrintedOnce
      then Just $ pure . inlinePrint fVar i printedBound
      else Nothing

-- every variable that is printed
printVars :: [Instruction] -> [Var]
printVars = concatMap $ \case
  PRINT x -> [x]
  _ -> []

inlinePrint :: Var -> Int -> [Var] -> IRProgram -> IRProgram
inlinePrint f i xs (is,ds,fs) =
  let
    is' = removeBinding $ removePrint is
    fs' = updateF f u fs
  in (is',ds,fs')
  where
    removePrint = concatMap $ \case
      IF c t e -> [IF c (removePrint t) (removePrint e)]
      PRINT x -> [ PRINT x | x `notElem` xs ]
      x -> [x]
    removeBinding = concatMap $ \case
      IF c t e -> [IF c (removeBinding t) (removeBinding e)]
      BINDCALL g ps rvs
        | g == f -> [BINDCALL f ps (dropIndex i rvs)]
        | otherwise -> [BINDCALL g ps rvs]
      x -> [x]
    u (fVar,(as,bs),is) = (fVar,(as,bs),addPrintYield is)
    addPrintYield = concatMap $ \case
      IF c t e -> [IF c (addPrintYield t) (addPrintYield e)]
      YIELD rvs ->
        let rvs' = dropIndex i rvs
        in PRINT (rvs !! i) : [ YIELD rvs' | not $ null rvs' ]
      x -> [x]

dropIndex :: Int -> [a] -> [a]
dropIndex i xs
  -- | 0 <= i && i < length xs = uncurry (++) . second tail $ splitAt i xs
  | 0 <= i && i < length xs = (\(xs,_,ys) -> xs ++ ys) $ peekAt i xs
  | otherwise = error "dropIndex: index out of bounds"

foldRewrite :: F -> Int ->  Rewrite FreshVarM
foldRewrite (f,(as,_),_) i = Rewrite t $ "inline 'external' accumulation for paramter " ++ show i ++ " of " ++ name f
  where
    t (is,ds,fs) =
      -- this condition is quite conservative right now to avoid complexity
      let bCs = bindCalls f is
          [(_,rvs)] = bCs
          isBaseParam = i < length as
          -- check how many different functions are used on the accumulation parameters inside the function
          -- if its just one (++) than we can apply the transformation
          notUsedOtherwise =  maybe False ((== ["++"]). nub . map printFlat . (\x -> usedOnBaseInScope (baseName x) (name f) ds) . (!! i) . fst . fst) $ lookupF f fs
          uc = fromMaybe 0 $ lookup (rvs !! i) $ usedVars ds
      in if length bCs == 1 && isBaseParam && notUsedOtherwise && uc == 1 && isJust (lookupF f fs)
        then Just $ foldOpt f i
        else Nothing

bindCalls :: Var -> [Instruction] -> [([Var],[Var])]
bindCalls f = concatMap @[] (\case BINDCALL g ps rvs -> [(ps,rvs) | f == g]; _ -> [])

-- "inlining" fold optimization
foldOpt :: Var -> Int -> IRProgram -> FreshVarM IRProgram
foldOpt _ _ ([],ds,fs) = return ([],ds,fs)
-- again, this pattern is conservative to reduce complexity
foldOpt f n (BINDCALL g (peekAt n -> (ps1,p,ps2)) rvs : PRINT t : is,ds,fs)
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

peekAt :: Int -> [a] -> ([a],a,[a])
peekAt i xs
  | 0 <= i && i < length xs =
    let (as,b:bs) = splitAt i xs
    in (as,b,bs)
  | otherwise = error "peekAt: index out of bounds"

data ChangeMode = Add Varname | Replace

-- NOTE: This is only sound if the accumulation parameter is not printed out!
toFoldAccum :: (AST Var -> AST Var -> AST Var) -> Var -> Int -> [F] -> [Def] -> FreshVarM [Def]
toFoldAccum g f i fs ds =
  let
    ((as,bs),b) = fromJust $ lookupF f fs
    xs = filterJust (inputDependency (as++bs) ds) (==((as++bs) !! i)) $ leavingVars b
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
extractAlgebra (App (Leaf f) (Var x)) rv p = do
  guard (x == rv)
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
    ((as,bs),_) <- lookupF f fs
    let usedOtherwise = any (any (`elem` map fst knownFolds)) $ map printFlat . (\x -> usedOnBaseInScope (baseName x) (name f) ds) <$> (as++bs)
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
introAccums gis ds (f,(as,bs),fis) = do
  (gis',ds',(bs',fis')) <- foldM (\(gis',ds',(bs',fis')) p -> do
      (ds',ivs,ys,fis') <- introSingleAccum ds' f p fis'
      let (is,ds'') = addArguments f ivs gis'
      return (is, ds' ++ ds'',(bs' ++ ys ,fis'))
    ) (gis,ds,(bs,fis)) as
  return (gis',ds',(f,(as,bs'),fis'))

introSingleAccum :: [Def] -> Var -> Var -> [Instruction] -> FreshVarM ([Def],[Maybe (Var -> Def)],[Var],[Instruction])
introSingleAccum ds fVar p is =
    foldM (\(ds',xs,ys,is') f ->
      case lookup f knownFolds of
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

-- first parameter are the basenames of the new accumulation parameter and the old one it is derived from
addNewParameter :: Var -> Var -> [Instruction] -> [Instruction]
addNewParameter new f = map $ \case
  READ x -> READ x
  PRINT x -> PRINT x
  IF c t e -> IF c (addNewParameter new f t) (addNewParameter new f e)
  TAILCALL g ps -> TAILCALL g (ps ++ [new | f == g ])
  BINDCALL g ps rvs -> BINDCALL g ps rvs
  YIELD x -> YIELD x
  NOP -> NOP

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
