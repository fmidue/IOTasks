{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.CodeGeneration.IR (
  -- IR (..),
  -- IRTag (..),
  -- irRead,
  -- irUpdate,
  -- irPrint,
  -- irIf,
  -- irDefLoop,
  -- irEnterLoop,
  -- irRecCall,
  -- irReturn,
  -- irNOP,
  -- printBasicProgram,
  -- tupelize,
  -- irmap,
  -- irfoldSpine,
  -- name,
  -- IndexedVar(..),
  ) where

import Data.Environment
import Data.Term
import Data.Term.AST

import Text.PrettyPrint.HughesPJClass (Doc, Pretty)
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.List (intercalate)

import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Tree.Pretty
import Data.Function (on)

import Data.MonoTraversable
import Control.Applicative (liftA2)

type Arg = AST IndexedVar

newtype IR = IR [IRTag] deriving Show
  deriving (Semigroup, Monoid) via [IRTag]

irmap :: (IRTag -> IRTag) -> IR -> IR
irmap _ (IR []) = IR []
irmap f (IR (x : xs)) = IR $ recomap f (f x) : omap f xs

recomap :: (IRTag -> IRTag) -> IRTag -> IRTag
recomap f (IF c t e) = IF c (irmap f t) (irmap f e)
recomap f (DEFLOOP l wVs p) = DEFLOOP l wVs (irmap f p)
recomap _ x = x

-- TODO: provide a version of this function that automatically
-- recurses into if-branches and loop bodys
irfoldSpine :: (IRTag -> b -> b) -> b -> IR -> b
irfoldSpine _ z (IR []) = z
irfoldSpine f z (IR (x:xs)) = f x (irfoldSpine f z $ IR xs)

instance Show (Varname -> Varname -> AST IndexedVar) where
  show f = show $ f "<xi>" "<v>"

newtype IndexedVar = IVar (Varname,Int) deriving (Eq,Ord,Show)

-- !!! not necessarily unique in the sense that one can define ("x1",1) and ("x",11)
name :: IndexedVar -> String
name (IVar (_,0)) = "[]" -- this feels wrong somehow
name (IVar (x,i)) = x ++ show i

data IRTag
  = READ IndexedVar
  | UPDATE IndexedVar (Varname -> Varname -> AST IndexedVar) IndexedVar IndexedVar -- UPDATE x f y v assigns the result f applied to y and v to x, ie. x := f y v
  | PRINT (AST IndexedVar)
  | IF (AST IndexedVar) IR IR
  | DEFLOOP IndexedVar [IndexedVar] IR -- define a loop
  | ENTERLOOP IndexedVar [Arg] [IndexedVar] -- enter/call a loop and bind the result
  | RECCALL IndexedVar [Arg] -- recursive call of the current loop
  | RETURN [IndexedVar] -- finish current loop
  deriving Show

instance Eq IRTag where
  READ x == READ y = x == y
  UPDATE x1 f x2 x3 == UPDATE y1 g y2 y3 = (x1,f (name x2) (name x3),x2,x3) == (y1,g (name y2) (name y3),y2,y3)
  PRINT x == PRINT y = x == y
  IF x1 x2 x3 == IF y1 y2 y3 = (x1,x2,x3) == (y1,y2,y3)
  DEFLOOP x1 x2 x3 == DEFLOOP y1 y2 y3 = (x1,x2,x3) == (y1,y2,y3)
  ENTERLOOP x1 x2 x3 == ENTERLOOP y1 y2 y3 = (x1,x2,x3) == (y1,y2,y3)
  RECCALL l x == RECCALL l' y = (l,x) == (l',y)
  RETURN x == RETURN y = x == y
  _ == _ = False

irRead :: IndexedVar -> IR
irRead x = IR [READ x]
irUpdate :: IndexedVar -> (Varname -> Varname -> AST IndexedVar) -> IndexedVar -> IndexedVar -> IR
irUpdate x f y v= IR [UPDATE x f y v]
irPrint :: AST IndexedVar -> IR
irPrint v = IR [PRINT v]
irIf :: AST IndexedVar -> IR -> IR -> IR
irIf c t e = IR [IF c t e]
irDefLoop :: IndexedVar -> [IndexedVar] -> IR -> IR
irDefLoop l wVs b = IR [DEFLOOP l wVs b]
irEnterLoop :: IndexedVar -> [Arg] -> [IndexedVar] -> IR
irEnterLoop l ps rVs = IR [ENTERLOOP l ps rVs]
irRecCall :: IndexedVar -> [Arg] -> IR
irRecCall l ps = IR [RECCALL l ps]
irReturn :: [IndexedVar] -> IR
irReturn rVs = IR [RETURN rVs]
irNOP :: IR
irNOP = IR []

infixr 5 :::
pattern x ::: y <- IR (x : (IR -> y))

pattern NOP <- IR []

instance Eq IR where
  (==) = (==) `on` show

instance Pretty IR where
  pPrint = PP.text . drawVerticalTree . toTree where
    toTree :: IR -> Tree String
    toTree = irfoldSpine toTree' (Tree.Node "NOP" [])
    toTree' (READ x) t' = Tree.Node ("READ " ++ name x) [] +++ t'
    toTree' (UPDATE xk f xi v) t' = Tree.Node ("UPDATE " ++ name xk ++ " := " ++ printAST (f (name xi) (name v))) [] +++ t'
    toTree' (PRINT ast) t' = Tree.Node ("PRINT " ++ printAST ast) [] +++ t'
    toTree' (IF c t e) t' = Tree.Node ("IF " ++ printAST c) [toTree t, toTree e] +++ t'
    toTree' (DEFLOOP ident writeVars p) t' = Tree.Node ("DEFLOOP " ++ name ident ++ show writeVars) [toTree p] +++ t'
    toTree' (ENTERLOOP ident params returnVars) t' = Tree.Node ("ENTERLOOP " ++ name ident ++ show params ++ show returnVars) [] +++ t'
    toTree' (RECCALL l x) t' = Tree.Node ("RECCALL " ++ name l ++ tupelize (map printAST x)) [] +++ t'
    toTree' (RETURN x) t' = Tree.Node ("RETURN " ++ tupelize (map name x)) [] +++ t'
    t +++ t' = Tree.Node "SEQ" [t,t']

printBasicProgram :: IR -> Doc
printBasicProgram = irfoldSpine (\x y -> printTag x PP.$$ y) PP.empty

printTag :: IRTag -> Doc
printTag (READ x) = PP.text $ name x ++ " <- readLn"
printTag (UPDATE xi f xk v) = PP.text $ "let " ++ name xi ++ " = " ++ printAST (f (name xk) (name v))
printTag (PRINT ast) = PP.text $ "print " ++ formatParam CallParam [printAST ast]
printTag (IF ast t e) =
  PP.hang (PP.text $ "if " ++ printAST ast) 2 $
     PP.hang (PP.text "then do") 2 (printBasicProgram t)
  PP.$$ PP.hang (PP.text "else do") 2 (printBasicProgram e)
printTag (DEFLOOP ident writeVars p) =
  PP.hang (PP.text $ "let " ++ name ident ++ " " ++ formatParam FunctionParam (map name writeVars) ++ " = do") 6
  (printBasicProgram p)
printTag (ENTERLOOP ident params []) =
  PP.text (name ident ++ " " ++ formatParam CallParam (map printAST params))
printTag (ENTERLOOP ident params returnVars) =
  PP.text (formatParam ReturnParam (map name returnVars)) PP.<+> PP.text ("<- " ++ name ident ++ " " ++ formatParam CallParam (map printAST params))
printTag (RECCALL l ps) = PP.text (name l) PP.<+> PP.text (formatParam CallParam (map printAST ps))
printTag (RETURN returnVars) = PP.text "return" PP.<+> PP.text (formatParam ReturnParam (map name returnVars))

data Mode = FunctionParam | CallParam | ReturnParam

formatParam :: Mode -> [String] -> String
formatParam FunctionParam xs = unwords xs
formatParam CallParam xs = unwords $ map (\x -> if length (words x) > 1 then "("++ x ++")" else x) xs
formatParam ReturnParam xs = tupelize xs

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"
