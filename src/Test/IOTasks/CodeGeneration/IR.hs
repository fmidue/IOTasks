{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.CodeGeneration.IR (
  IR (..),
  IRTag (..),
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
  irmap,
  irfoldSpine,
  ) where

import Test.IOTasks.Environment
import Test.IOTasks.Term

import Text.PrettyPrint.HughesPJClass (Doc, Pretty)
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.List (intercalate)

import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Tree.Pretty
import Data.Function (on)

import Data.MonoTraversable
import Control.Applicative (liftA2)

type Arg = String

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

instance Show (Varname -> Varname -> AST) where
  show f = show $ f "<xi>" "<v>"

data IRTag
  = READ Varname
  | UPDATE Varname (Varname -> Varname -> AST) Varname Varname -- UPDATE x f y v assigns the result f applied to y and v to x, ie. x := f y v
  | PRINT AST
  | IF AST IR IR
  | DEFLOOP Varname [Varname] IR -- define a loop
  | ENTERLOOP Varname [Arg] [Varname] -- enter/call a loop and bind the result
  | RECCALL [Varname] -- recursive call of the current loop
  | RETURN [Varname] -- finish current loop
  deriving Show

irRead :: Varname -> IR
irRead x = IR [READ x]
irUpdate :: Varname -> (Varname -> Varname -> AST) -> Varname -> Varname -> IR
irUpdate x f y v= IR [UPDATE x f y v]
irPrint :: AST -> IR
irPrint v = IR [PRINT v]
irIf :: AST -> IR -> IR -> IR
irIf c t e = IR [IF c t e]
irDefLoop :: Varname -> [Varname] -> IR -> IR
irDefLoop l wVs b = IR [DEFLOOP l wVs b]
irEnterLoop :: Varname -> [Arg] -> [Varname] -> IR
irEnterLoop l ps rVs = IR [ENTERLOOP l ps rVs]
irRecCall :: [Varname] -> IR
irRecCall ps = IR [RECCALL ps]
irReturn :: [Varname] -> IR
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
    toTree' (READ x) t' = Tree.Node ("READ " ++ x) [] +++ t'
    toTree' (UPDATE xk f xi v) t' = Tree.Node ("UPDATE " ++ xk ++ " := " ++ printAST (f xi v)) [] +++ t'
    toTree' (PRINT ast) t' = Tree.Node ("PRINT " ++ printAST ast) [] +++ t'
    toTree' (IF c t e) t' = Tree.Node ("IF " ++ printAST c) [toTree t, toTree e] +++ t'
    toTree' (DEFLOOP ident writeVars p) t' = Tree.Node ("DEFLOOP " ++ ident ++ show writeVars) [toTree p] +++ t'
    toTree' (ENTERLOOP ident params returnVars) t' = Tree.Node ("ENTERLOOP " ++ ident ++ show params ++ show returnVars) [] +++ t'
    toTree' (RECCALL x) t' = Tree.Node ("RECCALL " ++ tupelize x) [] +++ t'
    toTree' (RETURN x) t' = Tree.Node ("RETURN " ++ tupelize x) [] +++ t'
    t +++ t' = Tree.Node "SEQ" [t,t']

printBasicProgram :: IR -> Doc
printBasicProgram = irfoldSpine (\x y -> printTag x PP.$$ y) PP.empty

printTag :: IRTag -> Doc
printTag (READ x) = PP.text $ x ++ " <- readLn"
printTag (UPDATE xi f xk v) = PP.text $ "let " ++ xi ++ " = " ++ printAST (f xk v)
printTag (PRINT ast) = PP.text $ "print " ++ formatParam CallParam [printAST ast]
printTag (IF ast t e) =
  PP.hang (PP.text $ "if " ++ printAST ast) 2 $
     PP.hang (PP.text "then do") 2 (printBasicProgram t)
  PP.$$ PP.hang (PP.text "else do") 2 (printBasicProgram e)
printTag (DEFLOOP ident writeVars p) =
  PP.hang (PP.text $ "let " ++ ident ++ " " ++ formatParam FunctionParam writeVars ++ " = do") 6
  (printBasicProgram p)
printTag (ENTERLOOP ident params []) =
  PP.text (ident ++ " " ++ formatParam CallParam params)
printTag (ENTERLOOP ident params returnVars) =
  PP.text (formatParam ReturnParam returnVars) PP.<+> PP.text ("<- " ++ ident ++ " " ++ formatParam CallParam params)
printTag (RECCALL xs) = PP.text "loop" PP.<+> PP.text (formatParam CallParam xs)
printTag (RETURN returnVars) = PP.text "return" PP.<+> PP.text (formatParam ReturnParam returnVars)

data Mode = FunctionParam | CallParam | ReturnParam

formatParam :: Mode -> [String] -> String
formatParam FunctionParam xs = unwords xs
formatParam CallParam xs = unwords $ map (\x -> if length (words x) > 1 then "("++ x ++")" else x) xs
formatParam ReturnParam xs = tupelize xs

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"
