{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.CodeGeneration.IRGraph (
  IRSpine(..),
  Spine(..),
  SpineFold(..),
  Def,
  DefRhs(..),
  Expr,
  foldrSpine,
  updateDef,
  mapRhs,
  (<:>),
  irRead,
  irUpdate,
  irPrint,
  irIf,
  irDefLoop,
  irEnterLoop,
  irRecCall,
  irReturn,
  irNOP,
  printBasicProgram,
  tupelize,
  name,
  IndexedVar(..),
  ) where

import Data.Environment
import Data.Term.AST

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.List (intercalate,sort,group)

import Control.Arrow ((&&&),second)

type Expr = AST IndexedVar

data IRSpine = IRSpine { spine :: Spine, defs :: [Def] } deriving (Eq,Show)

-- irmap :: (IRTag -> IRTag) -> IR -> IR
-- irmap _ (IR []) = IR []
-- irmap f (IR (x : xs)) = IR $ recomap f (f x) : omap f xs
--
-- recomap :: (IRTag -> IRTag) -> IRTag -> IRTag
-- recomap f (IF c t e) = IF c (irmap f t) (irmap f e)
-- recomap f (DEFLOOP l wVs p) = DEFLOOP l wVs (irmap f p)
-- recomap _ x = x

data SpineFold b = SpineFold {
  fRead :: IndexedVar -> b -> b,
  fPrint :: Expr -> b -> b,
  fEnterLoop :: IndexedVar -> [Expr] -> [IndexedVar] -> b -> b,
  fIf :: Expr -> b -> b -> b,
  fReturn :: [IndexedVar] -> b,
  fRecCall :: IndexedVar -> [Expr] -> b,
  fNop :: b
  }

foldrSpine :: ([Def] -> SpineFold b) -> IRSpine -> b
foldrSpine f (IRSpine s ds) = foldSpine' (f ds) s where
  foldSpine' :: SpineFold b -> Spine -> b
  foldSpine' phi@SpineFold{..} = \case
    (READ v s') -> fRead v (foldSpine' phi s')
    (PRINT ast s') -> fPrint ast (foldSpine' phi s')
    (ENTERLOOP l ps rVs s') -> fEnterLoop l ps rVs (foldSpine' phi s')
    (IF c s1 s2) -> fIf c (foldSpine' phi s1) (foldSpine' phi s2)
    (RETURN rVs) -> fReturn rVs
    (RECCALL l ps) -> fRecCall l ps
    NOP -> fNop


-- TODO: provide a version of this function that automatically
-- recurses into if-branches and loop bodys
-- irfoldSpine :: (IRTag -> b -> b) -> b -> IR -> b
-- irfoldSpine _ z (IR []) = z
-- irfoldSpine f z (IR (x:xs)) = f x (irfoldSpine f z $ IR xs)

instance Show v => Show (Varname -> Varname -> AST v) where
  show f = show $ f "<xi>" "<v>"

newtype IndexedVar = IVar (Varname,Int) deriving (Eq,Ord,Show)

-- !!! not necessarily unique in the sense that one can define ("x1",1) and ("x",11)
name :: IndexedVar -> String
name (IVar (_,0)) = "[]" -- this feels wrong somehow
name (IVar (x,i)) = x ++ show i

data Spine
  = READ IndexedVar Spine
  | PRINT Expr Spine
  | ENTERLOOP IndexedVar [Expr] [IndexedVar] Spine -- enter/call a loop and bind the result
  | IF Expr Spine Spine
  | RETURN [IndexedVar] -- finish current loop
  | RECCALL IndexedVar [Expr] -- recursive call of the current loop
  | NOP -- empty program/spine end
  deriving (Eq,Show)

-- non monadic bindings, i.e., let x = rhs
type Def = (IndexedVar, DefRhs)

data DefRhs
  = UPDATE (Varname -> Varname -> Expr) IndexedVar IndexedVar -- UPDATE x f y v assigns the result f applied to y and v to x, ie. x := f y v
  | DEFLOOP [IndexedVar] IRSpine -- define a loop
  deriving Show

instance Eq DefRhs where
  UPDATE f x1 x2 == UPDATE g y1 y2 = (f (name x1) (name x2),x1,x2) == (g (name y1) (name y2),y1,y2)
  DEFLOOP x1 x2 == DEFLOOP y1 y2 = (x1,x2) == (y1,y2)
  _ == _ = False

updateDef :: IndexedVar -> (DefRhs -> DefRhs) -> [Def] -> [Def]
updateDef _ _ [] = []
updateDef x f ((y,rhs):xs)
  | x == y = (x,f rhs) : xs
  | otherwise = (y,rhs) : updateDef x f xs

mapRhs :: (DefRhs -> DefRhs) -> [Def] -> [Def]
mapRhs f = map (second f)

-- combine two IRSpine values
-- Assumption: the definition partse of both are disjpint
(<:>) :: IRSpine -> IRSpine -> IRSpine
IRSpine s1 ds1 <:> IRSpine s2 ds2 = IRSpine (ins s1) (ds1 ++ ds2) where
  ins :: Spine -> Spine
  ins (READ v s') = READ v (ins s')
  ins (PRINT ast s') = PRINT ast (ins s')
  ins (ENTERLOOP l ps rVs s') = ENTERLOOP l ps rVs (ins s')
  ins (IF c t e) = IF c (ins t) (ins e)
  ins (RETURN rVs) = RETURN rVs
  ins (RECCALL l ps) = RECCALL l ps
  ins NOP = s2

-- computes the variables used on the spine and their usage frequency
spineVars :: IRSpine -> [(IndexedVar,Int)]
spineVars (IRSpine s ds) = map (head &&& length) . group . sort $ vars s where
  vars :: Spine -> [IndexedVar]
  vars (READ _ s') = vars s'
  vars (PRINT t s') = astVars t ++ vars s'
  vars (ENTERLOOP l ps _ s') = l : concatMap astVars ps ++ vars s'
  vars (IF c t e) = astVars c ++ vars t ++ vars e
  vars (RETURN rVs) = rVs
  vars (RECCALL l ps) = l : concatMap astVars ps
  vars NOP = []

-- compute the variables used in rhs of definitions and their frequency
defVars :: IRSpine -> [(IndexedVar,Int)]
defVars (IRSpine _ ds) = map (head &&& length) . group . sort $ concatMap (vars . snd) ds where
  vars :: DefRhs -> [IndexedVar]
  vars (UPDATE f x1 x2) = astVars (f (name x1) (name x2))
  vars (DEFLOOP _ b) = map fst $ spineVars b

-- try to reduce the number of definitions by inlining intermediate values
inlineDef :: IRSpine -> IRSpine
inlineDef (IRSpine s ds) =
  let
  -- 1. inline inside definitions
  -- 2. inline (minimized) definitions into spine
  in _

f :: Def -> Def -> Def
f (x,rhs) (y,UPDATE f x1 x2)
  | x == x1 = (y,UPDATE _ _ x2)
  | x == x2 = (y,_)
  | otherwise = (y,UPDATE f x1 x2)
f (x,rhs) (y,DEFLOOP rhs'1 rhs'2) = _

irRead :: IndexedVar -> IRSpine
irRead x = IRSpine (READ x NOP) []
irUpdate :: IndexedVar -> (Varname -> Varname -> Expr) -> IndexedVar -> IndexedVar -> IRSpine
irUpdate x f y v = IRSpine NOP [(x,UPDATE f y v)]
irPrint :: Expr -> IRSpine
irPrint v = IRSpine (PRINT v NOP) []
irIf :: Expr -> IRSpine -> IRSpine -> IRSpine
irIf c (IRSpine t ds1) (IRSpine e ds2) = IRSpine (IF c t e) (ds1 ++ ds2)
irDefLoop :: IndexedVar -> [IndexedVar] -> IRSpine -> IRSpine
irDefLoop l wVs ir = IRSpine NOP [(l,DEFLOOP wVs ir)]
irEnterLoop :: IndexedVar -> [Expr] -> [IndexedVar] -> IRSpine
irEnterLoop l ps rVs = IRSpine (ENTERLOOP l ps rVs NOP) []
irRecCall :: IndexedVar -> [Expr] -> IRSpine
irRecCall l ps = IRSpine (RECCALL l ps) []
irReturn :: [IndexedVar] -> IRSpine
irReturn rVs = IRSpine (RETURN rVs) []
irNOP :: IRSpine
irNOP = IRSpine NOP []

-- infixr 5 :::
-- pattern x ::: y <- IR (x : (IR -> y))
--
-- pattern NOP <- IR []

printBasicProgram :: IRSpine -> Doc
printBasicProgram (IRSpine s ds) =
  PP.text "Spine:" PP.$$
  printSpine s PP.$$
  PP.text "\nDefinitions:" PP.$$
  PP.vcat (map printDef ds)

printSpine :: Spine -> Doc
printSpine (READ x s') =
  PP.text (name x ++ " <- readLn")
    PP.$$ printSpine s'
printSpine (PRINT ast s') =
  PP.text ("print " ++ formatParam CallParam [printAST ast])
    PP.$$ printSpine s'
printSpine (IF ast t e) =
  PP.hang (PP.text $ "if " ++ printAST ast) 2
    (PP.text "then" PP.<+> printSpine t
    PP.$$ PP.text "else" PP.<+> printSpine e)
printSpine (ENTERLOOP ident params [] s') =
  PP.text (name ident ++ " " ++ formatParam CallParam (map printAST params))
    PP.$$ printSpine s'
printSpine (ENTERLOOP ident params returnVars s') =
  PP.text (formatParam ReturnParam (map name returnVars)) PP.<+> PP.text ("<- " ++ name ident ++ " " ++ formatParam CallParam (map printAST params))
    PP.$$ printSpine s'
printSpine (RECCALL l ps) = PP.text (name l) PP.<+> PP.text (formatParam CallParam (map printAST ps))
printSpine (RETURN returnVars) = PP.text "return" PP.<+> PP.text (formatParam ReturnParam (map name returnVars))
printSpine NOP = PP.empty

printDef :: Def -> Doc
printDef (xi, UPDATE f xk v) = PP.text $ "let " ++ name xi ++ " = " ++ printAST (f (name xk) (name v))
printDef (ident, DEFLOOP writeVars b) =
  PP.hang (PP.text $ "let " ++ name ident ++ " " ++ formatParam FunctionParam (map name writeVars) ++ " = do") 6
    (printBasicProgram b)

data Mode = FunctionParam | CallParam | ReturnParam

formatParam :: Mode -> [String] -> String
formatParam FunctionParam xs = unwords xs
formatParam CallParam xs = unwords $ map (\x -> if length (words x) > 1 then "("++ x ++")" else x) xs
formatParam ReturnParam xs = tupelize xs

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"
