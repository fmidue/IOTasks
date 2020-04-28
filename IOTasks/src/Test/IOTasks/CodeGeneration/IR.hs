{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.IR where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (sort, group, intercalate)
import Data.Proxy
import Type.Reflection (Typeable, eqTypeRep, typeRep, (:~~:)(..))
import Control.Monad.State (StateT(..), MonadState, get)
import Data.Functor.Identity (Identity(..))

import Data.Environment
import Data.Term
import Data.Term.Liftable
import qualified Data.Term.Liftable.Prelude as T
import Data.Term.Typed.AST

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP

import Debug.Trace

data Instruction
  = READ Var
  | PRINT Var
  | IF Var [Instruction] [Instruction]
  | TAILCALL Var [Var] -- TAILCALL f [x1,..,xn] => f x1 .. xn
  | BINDCALL Var [Var] [Var] -- BINDCALL f [x1,..,xn] [v1,..,vm] => (v1,..,vm) <- f x1 .. xn
  | YIELD [Var]
  | NOP
  deriving (Show,Eq)

type Var = String

type AM = ([Instruction],([Def ()],[F]),Vals,[[Var]],[Input],[Output])

type IRProgram = ([Instruction], [Def ()], [F])
type Vals = Environment Var
type Def a = (Var,DefRhs a,Int)
data DefRhs :: * -> * where
  Forget :: Typeable a => DefRhs a -> DefRhs ()
  U :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> Var -> Var -> DefRhs c
  N1L :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> DefRhs a -> Var -> DefRhs c
  N1R :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> Var -> DefRhs b -> DefRhs c
  N2  :: forall c a b. (Typeable a, Typeable b) => (AST Var a -> AST Var b -> AST Var c) -> DefRhs a -> DefRhs b -> DefRhs c
  Const :: AST Var a -> DefRhs a
type F = (Var, [Var], [Instruction])
type Input = Int
data Output = I Int | O Int deriving (Show,Eq) -- simple trace elements

-- very basic sequencing, assumes well-formed input, i.e. not duplicate varaiable definitions etc.
(<:>) :: IRProgram -> IRProgram -> IRProgram
(is1,ds1,fs1) <:> (is2,ds2,fs2) = (is1 ++ is2,foldr (\(x,d,_) -> addDef (x,d)) ds1 ds2,fs1 ++ fs2)

readIR :: Var -> IRProgram
readIR x = ([READ x],[],[])

initialValueIR :: Typeable a => Var -> Var -> Var -> Proxy a -> IRProgram
initialValueIR x y v (Proxy :: Proxy a) = ([],[(x,Forget $ U @[a] @[a] @a (const (`T.cons` T.nil)) y v,0)],[])

updateIR :: Typeable a => Var -> Var -> Var -> Proxy a -> IRProgram
updateIR x y v (Proxy :: Proxy a) = ([],[(x, Forget $ U @[a] (\y v -> y T.++ (v `T.cons` T.nil)) y v,0)],[])

printIR :: Var -> IRProgram
printIR x = ([PRINT x],[],[])

valueDefIR :: Typeable a => Var -> AST Varname a -> IRProgram
valueDefIR x t = ([],[(x,Forget $ Const t,0)],[])

ifIR :: Var -> IRProgram -> IRProgram -> IRProgram
ifIR c (is1,ds1,fs1) (is2,ds2,fs2) = ([IF c is1 is2],ds1 ++ ds2,fs1 ++ fs2)

defLoopIR :: Var -> [Var] -> IRProgram -> IRProgram
defLoopIR f xs (b,ds,fs) = ([],ds,(f,xs,b):fs)

enterLoopIR :: Var -> [Var] -> [Var] -> IRProgram
enterLoopIR f ps rvs = ([BINDCALL f ps rvs],[],[])

recCallIR :: Var -> [Var] -> IRProgram
recCallIR f ps = ([TAILCALL f ps],[],[])

yieldIR :: [Var] -> IRProgram
yieldIR rvs = ([YIELD rvs],[],[])

nopIR :: IRProgram
nopIR = ([NOP],[],[])

runInstructions :: [Instruction] -> ([Def ()],[F]) -> [Input] -> [Output]
runInstructions p d i = let (_,_,_,_,_,o) = runAM (p,d,emptyEnvironment,[],i,[]) in o

runAM :: AM -> AM
runAM ([],d,vs,rvs,i,o) = ([],d,vs,rvs,i,o)
runAM m = runAM $ stepAM m

stepAM :: AM -> AM
stepAM ([],_,_,_,_,_) = error "no instruction to execute"
stepAM (READ _:_,_,_,_,[],_) = error "not enough inputs"
stepAM (READ x:p',d,vs,rvs,i:is,o) = (p',d,updateValue (x,i) vs,rvs,is,I i:o)
stepAM (PRINT x:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,O (_intValue x d vs):o)
stepAM (IF x t e:p',d,vs,rvs,i,o)
  | _boolValue x d vs = (t++p',d,vs,rvs,i,o)
  | otherwise = (e++p',d,vs,rvs,i,o)
stepAM (TAILCALL l ps:p',d,vs,rvs,i,o) = (_getInstructions l d ++ p',d,_setParameters l ps d vs,rvs,i,o)
stepAM (BINDCALL l ps bs:p',d,vs,rvs,i,o) = (_getInstructions l d ++ p',d,_setParameters l ps d vs,bs:rvs,i,o)
stepAM (YIELD _:_,_,_,[],_,_) = error "missing binders for returned values"
stepAM (YIELD xs:p',d,vs,rvs:rvss,i,o)
  | _length vs == length rvs = (p', d,updateValues (zipWith (\x rv -> (rv,_getPayload x vs)) xs rvs) vs,rvss,i,o)
  | otherwise = error "wrong number of binders for returned values"
stepAM (NOP:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,o)

updateValues :: [(Var, a)] -> Vals -> Vals
updateValues xs ys = fromJust $ foldr _ (Just ys) xs

updateValue :: (Typeable a, Show a) => (Var,a) -> Vals -> Vals
updateValue (x,v) = fromJust . store x show v

-- printing programs
printIRProgram :: IRProgram -> Doc
printIRProgram (is,ds,fs) =
        PP.text "-- Main --"
  PP.$$ PP.vcat (map printInstruction is)
  PP.$$ PP.text "-- Defs --"
  PP.$$ PP.vcat (map printDef ds)
  PP.$$ PP.text "-- Loops --"
  PP.$$ PP.vcat (map printF fs)

printInstruction :: Instruction -> Doc
printInstruction (READ x) = PP.text $ "READ " ++ x
printInstruction (PRINT t) = PP.text $ "PRINT " ++ t
printInstruction (IF c t e) =
  PP.hang (PP.text ("IF " ++ c)) 2
    ( PP.hang (PP.text "THEN") 2 (PP.vcat (map printInstruction t))
    PP.$$ PP.hang (PP.text "ELSE") 2 (PP.vcat (map printInstruction e))
    )
printInstruction (TAILCALL f ps) = PP.text ("TAILCALL " ++ f) PP.<+> tupelize ps
printInstruction (BINDCALL f ps rvs) = PP.text ("BINDCALL " ++ f) PP.<+> tupelize ps PP.<+> tupelize rvs
printInstruction (YIELD rvs) = PP.text "RETURN " PP.<+> tupelize rvs
printInstruction NOP = mempty

printDef :: Def a -> Doc
printDef (x,rhs,_) = PP.text (x ++ " :=") PP.<+> printDefRhs rhs

printDefRhs :: DefRhs a -> Doc
printDefRhs (Forget d) = printDefRhs d
printDefRhs (U f y v) = PP.text $ printFlat' $ f (Leaf undefined y) (Leaf undefined v)
printDefRhs (N1L f y v) = PP.text $ printFlat' $ f (toAST y) (Leaf undefined v)
printDefRhs (N1R f y v) = PP.text $ printFlat' $ f (Leaf undefined y) (toAST v)
printDefRhs (N2 f y v) = PP.text $ printFlat' $ f (toAST y) (toAST v)
printDefRhs (Const t) = PP.text $ printFlat' t

printF :: F -> Doc
printF (f,xs,b) = PP.hang (PP.text f PP.<+> tupelize xs PP.<+> PP.text ":=") 2 $
  PP.vcat (map printInstruction b)

tupelize :: [String] -> Doc
tupelize [] = PP.text ""
tupelize [v] = PP.text v
tupelize vs = PP.text $ "(" ++ intercalate "," vs ++ ")"

-- manipulation of DefRhs
printDefTree :: DefRhs a -> String
printDefTree = pPrintTree . toAST

toAST :: DefRhs a -> AST Var a
toAST (Forget x) = App (unHO $ \_ -> Leaf () "()") $ toAST x
toAST (U f y v) = f (variable y) (variable v)
toAST (N1L f y v) = f (toAST y) (variable v)
toAST (N1R f y v) = f (variable y) (toAST v)
toAST (N2 f y v) = f (toAST y) (toAST v)
toAST (Const t) = t

instance Show (DefRhs a) where
  show (Forget x) = "Forget " ++ show x
  show (U f y v) = "U ? " ++ y ++ " " ++ v
  show (N1L f y v) = "N1L ? " ++ v
  show (N1R f y v) = "N1R ? " ++ y
  show (N2 f y v) = "N2 ?"
  show (Const t) = "Const ?"

addDef :: (Var,DefRhs a) -> [Def a] -> [Def a]
addDef (x,d) ds =
  let n = fromMaybe 0 $ lookup x $ usedVars (map (\(_,d,_) -> d) ds)
  in (x,d,n) : updateUseCount (usedVars [d]) ds

updateUseCount :: [(Var,Int)] -> [Def a] -> [Def a]
updateUseCount cs = foldr phi [] where
  phi (x,d,n) ds = case lookup x cs of
    Just m -> (x,d,n+m) : ds
    Nothing -> (x,d,n) : ds

definedVars :: [Def a] -> [Var]
definedVars = map (\(x,_,_) -> x)

usedVars :: [DefRhs a] -> [(Var,Int)]
usedVars = map (\x -> (head x, length x)) . group . sort . concatMap go where
  go :: DefRhs a -> [Var]
  go (Forget d) = go d
  go (U _ y v) = [y,v]
  go (N1L _ y v) = v : go y
  go (N1R _ y v) = y : go v
  go (N2 _ y v) = go y ++ go v
  go (Const t) = termVars t

lookup2 :: Eq k => k -> [(k,a,b)] -> Maybe (a,b)
lookup2 x ds = lookup x (map (\(x,d,n) -> (x,(d,n))) ds)

lookupDef :: Var -> [Def a] -> Maybe (DefRhs a,Int)
lookupDef = lookup2

updateDef :: (Var,DefRhs a) -> [Def a] -> [Def a]
updateDef (x,newRhs) = (\(ds',vs) -> updateUseCount vs ds') . updateDef' ([],[]) where
  updateDef' (ds',vs) [] = (ds',vs)
  updateDef' (ds',vs) ((y,oldRhs,n):ds)
    | x == y =
      let
        newUsed = usedVars [newRhs]
        oldUsed = usedVars [oldRhs]
        diff = usageDiff newUsed oldUsed
      in (ds'++(x,newRhs,n):ds,diff)
    | otherwise = updateDef' ((y,oldRhs,n):ds',vs) ds

-- 1st arg = new, 2nd arg = old
-- assumes ordered lists (like the ones produced by usedVars)
usageDiff :: [(Var,Int)] -> [(Var,Int)] -> [(Var,Int)]
usageDiff [] [] = []
usageDiff xs [] = xs
usageDiff [] ys = map (\(y,m) -> (y, negate m)) ys
usageDiff ((x,n):xs) ((y,m):ys)
  | x == y = (x,n-m) : usageDiff xs ys
  | x > y = (y,negate m) : usageDiff ((x,n):xs) ys
  | otherwise = (x,n) : usageDiff xs ((y,m):ys)

lookupF :: Var -> [F] -> Maybe ([Var],[Instruction])
lookupF = lookup2

updateF :: Var -> ([Instruction] -> [Instruction]) -> [F] -> [F]
updateF _ _ [] = []
updateF f bf ((g,ps,bg):fs)
  | f == g = (f,ps,bf bg) : fs
  | otherwise = (g,ps,bg) : updateF f bf fs

-- printing to Haskell
newtype ScopeM a = ScopeM { runScopeM :: [Var] -> (a, [Var]) }
  deriving (Functor, Applicative, Monad, MonadState [Var]) via (StateT [Var] Identity)

evalScopeM :: ScopeM a -> [Var] -> a
evalScopeM m = fst . runScopeM m

haskellCode :: IRProgram -> Doc
haskellCode (is,ds,fs) =
        PP.text "prog :: IO ()"
  PP.$$ PP.hang (PP.text "prog = do") 2
    (PP.vcat $ evalScopeM (mapM (renderInstruction ds fs) is) [])

renderInstruction :: [Def ()] -> [F] -> Instruction -> ScopeM Doc
renderInstruction _ _ (READ x) = return $ PP.text $ x ++ " <- readLn"
renderInstruction ds fs (PRINT t) = do
  (argTerm, ctx) <- renderVar ds t
  return $ ctx PP.$$ PP.text "print" PP.<+> argTerm
renderInstruction ds fs (IF c t e) = do
  (condition, ctx) <- renderVar ds c
  thenBranch <- PP.vcat <$> mapM (renderInstruction ds fs) t
  elseBranch <- PP.vcat <$> mapM (renderInstruction ds fs) e
  return $ ctx PP.$$ PP.hang (PP.text "if" PP.<+> condition) 2
    (       PP.hang (PP.text "then do") 2 thenBranch
      PP.$$ PP.hang (PP.text "else do") 2 elseBranch
    )
renderInstruction ds _ (TAILCALL f ps) = do
  (params,ctx) <- renderVars ds ps
  return $ ctx PP.$$ PP.text f PP.<+> PP.hcat params
renderInstruction ds fs (BINDCALL f ps rvs) = do
  loopDef <- renderLoop f ds fs
  (params, ctx) <- renderVars ds ps
  return $
    -- insert loop definition
    loopDef
    -- instert missing definitions
    PP.$$ ctx
    -- actual call
    PP.$$ (if null rvs then id else (tupelize rvs PP.<+> PP.text "<-" PP.<+>)) (PP.text f PP.<+> PP.hcat params)
renderInstruction ds _ (YIELD rvs) = do
  (params, ctx) <- renderVars ds rvs
  return $ ctx PP.$$ PP.text "return" PP.<+> PP.hcat params
renderInstruction _ _ NOP = return mempty

renderLoop :: Var -> [Def ()] -> [F] -> ScopeM Doc
renderLoop f ds fs = case lookupF f fs of
  Just (ps,is) -> do
    body <- PP.vcat <$> mapM (renderInstruction ds fs) is
    return $ PP.hang (PP.text $ "let " ++ f ++ " " ++ unwords ps ++ " =") 6 body
  Nothing -> error $ "can't find definition for " ++ f

renderVars :: [Def ()] -> [Var] -> ScopeM ([Doc], Doc)
renderVars _ [] = return ([],mempty)
renderVars ds (v:vs) = do
  (x,ctx) <- renderVar ds v
  (xs,ctxs) <- renderVars ds vs
  return (x:xs, ctx PP.$$ ctxs)

-- returns the a Doc for the actual variable and one wiht the neccessary definitions
renderVar :: [Def ()] -> Var -> ScopeM (Doc, Doc)
renderVar ds x = do
  ctx <- renderContext ds (neededVars ds x)
  case lookupDef x ds of
    Just (rhs,0) -> return (PP.parens (printDefRhs rhs), ctx)
    Just (rhs,1) -> return (PP.parens (printDefRhs rhs), ctx)
    Just _ -> return (PP.text x, ctx)
    Nothing -> return (PP.text x, ctx)

neededVars :: [Def ()] -> Var -> [Var]
neededVars ds x = case lookupDef x ds of
  Just (rhs,_) -> let us = map fst (usedVars [rhs]) in us ++ concatMap (neededVars ds) us
  Nothing -> []

renderContext :: [Def ()] -> [Var] -> ScopeM Doc
renderContext ds xs = do
  scope <- get
  let notDef = foldr (\x ys -> if x `notElem` scope then maybe ys (\v -> (x,fst v):ys) $ lookupDef x ds else ys) [] xs
  return $ PP.hcat $ map (uncurry renderAssignment) notDef

renderAssignment :: Var -> DefRhs a -> Doc
renderAssignment x rhs = PP.text ("let " ++ x ++ " =") PP.<+> printDefRhs rhs
