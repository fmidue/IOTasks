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
module Test.IOTasks.CodeGeneration.IRNew where

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
updateIR x y v (Proxy :: Proxy a) = ([],[(x, Forget $ U @[a] (T.++) y v,0)],[])

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

-- optimizing IRProgram
optimize :: IRProgram -> IRProgram
optimize (i,d,f) = (i,d',f)
  where d' = simplify (emptyEnvironment @Environment) d

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

lookupF :: Var -> [F] -> Maybe ([Var],[Instruction])
lookupF = lookup2

updateF :: Var -> [Instruction] -> [F] -> [F]
updateF _ _ [] = []
updateF f bf ((g,ps,bg):fs)
  | f == g = (f,ps,bf) : fs
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

-- further optimizations

-- NOTE: there is a naming issue here if we do this for a loop with more then one
-- retrun point (or a program that uses the name t', which should not happen, because of IndexedVar)
inlinePrint :: IRProgram -> IRProgram
inlinePrint ([],ds,fs) = ([],ds,fs)
inlinePrint p@(BINDCALL f ps [rv] : PRINT t : is,ds,fs) =
  case lookupDef t ds of
    Just (Forget (Const tRhs),n) | n <= 1 && termVars tRhs == [rv] ->
      let
        r y = mapV (\x -> if x == rv then y else x) tRhs
        (_,b) = fromJust $ lookupF f fs
        (b',ds',fs') = transformProgram tr (b,ds,fs)
        tr ds fs = (idFold ds fs){ fYield = trYield ds fs }
        trYield ds fs [rv] = ([PRINT (t++"'")], (t++"'",Forget $ Const (r rv),1) : ds ,fs)
        trYield ds fs rvs = ([YIELD rvs], ds, fs)
      in (BINDCALL f ps [] : is,ds',updateF f b' fs')
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

-- "inlining" fold optimization sketch
opt :: IRProgram -> IRProgram
opt ([],ds,fs) = ([],ds,fs)
opt (BINDCALL f ps [rv] : PRINT t : is,ds,fs) =
  case lookupDef t ds of
    Just (Forget (rhs :: DefRhs a),1) -> case _extractAlgebra (toAST rhs) rv of
      Just (g,_) -> ([BINDCALL f ps [_], PRINT rv],(_,_,1) : _toFoldAccum g f fs ds,fs)
      _ -> _
    _ -> _
opt (i:is,ds,fs) = let (is',ds',fs') = opt (is,ds,fs) in (i:is',ds',fs')

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
extractAlgebra :: (Typeable a, Typeable b) => AST Var b -> Var -> Maybe (AST Var a -> AST Var b -> AST Var b, AST Var b)
extractAlgebra (App (Leaf _ "sum") (Var x _)) y | x == y = Just (_,_)
extractAlgebra _ _ = Nothing
