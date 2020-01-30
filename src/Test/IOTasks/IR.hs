{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.IR where

import Test.IOTasks.Environment
import Test.IOTasks.Term

import Text.PrettyPrint.HughesPJClass
import Data.List (intercalate)

import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Tree.Pretty
import Data.Function (on)

data Level = Top | Body

type Arg = String

instance Show (Varname -> Varname -> AST) where
  show f = show $ f "<xi>" "<v>"

data IR l where
  READ :: Varname -> IR l
  UPDATE :: Varname -> (Varname -> Varname -> AST) -> Varname -> Varname -> IR l -- UPDATE x f y v assigns the result f applied to y and v to x, ie. x := f y v
  PRINT :: AST -> IR l
  IF :: AST -> IR l -> IR l -> IR l
  DEFLOOP :: [Varname] -> [Arg] -> [Varname] -> IR 'Body -> IR l -- define and call a loop
  CALLLOOP :: [Varname] -> IR 'Body -- recursive call of the current loop
  RETURN :: [Varname] -> IR 'Body -- finish current loop
  SEQ :: IR l -> IR l -> IR l
  NOP :: IR l

deriving instance Show (IR l)

instance Eq (IR l) where
  (==) = (==) `on` show

topToBody :: IR 'Top -> IR 'Body
topToBody (READ x) = READ x
topToBody (UPDATE x1 x2 x3 x4) = UPDATE x1 x2 x3 x4
topToBody (PRINT x) = PRINT x
topToBody (IF x1 x2 x3) = IF x1 (topToBody x2) (topToBody x3)
topToBody (DEFLOOP x1 x2 x3 x4) = DEFLOOP x1 x2 x3 x4
topToBody (SEQ x1 x2) = SEQ (topToBody x1) (topToBody x2)
topToBody NOP = NOP

instance Pretty (IR l) where
  pPrint = text . drawVerticalTree . toTree where
    toTree :: IR l -> Tree String
    toTree (READ x) = Tree.Node ("READ " ++ x) []
    toTree (UPDATE xk f xi v) = Tree.Node ("UPDATE " ++ xk ++ " := " ++ printAST (f xi v)) []
    toTree (PRINT ast) = Tree.Node ("PRINT " ++ printAST ast) []
    toTree (IF c t e) = Tree.Node ("IF " ++ printAST c) [toTree t, toTree e]
    toTree (DEFLOOP writeVars params returnVars p) = Tree.Node ("DEFLOOP " ++ show writeVars ++ show params ++ show returnVars) [toTree p]
    toTree (CALLLOOP x) = Tree.Node ("CALLLOOP " ++ tupelize x) []
    toTree (RETURN x) = Tree.Node ("RETURN " ++ tupelize x) []
    toTree (SEQ x1 x2) = Tree.Node "SEQ" (toTree <$> [x1,x2])
    toTree NOP = Tree.Node "NOP" []

printBasicProgram :: IR l -> Doc
printBasicProgram (READ x) = text $ x ++ " <- readLn"
printBasicProgram (UPDATE xi f xk v) = text $ "let " ++ xi ++ " = " ++ printAST (f xk v)
printBasicProgram (PRINT ast) = text $ "print (" ++ printAST ast ++ ")"
printBasicProgram (IF ast t e) =
  hang (text $ "if " ++ printAST ast) 2 $
     hang (text "then do") 2 (printBasicProgram t)
  $$ hang (text "else do") 2 (printBasicProgram e)
printBasicProgram (DEFLOOP writeVars params [] p) =
  hang (text $ "let loop " ++ unwords writeVars ++ " = do") 6
  (printBasicProgram p)
  $$ text ("loop " ++ unwords params)
printBasicProgram (DEFLOOP writeVars params returnVars p) =
  hang (text $ "let loop " ++ unwords writeVars ++ " = do") 6
  (printBasicProgram p)
  $$ text (tupelize returnVars) <+> text ("<- loop " ++ unwords params)
printBasicProgram (CALLLOOP xs) = text "loop" <+> text (tupelize xs)
printBasicProgram (RETURN returnVars) = text "return" <+> text (tupelize returnVars)
printBasicProgram (SEQ x y) = printBasicProgram x $$ printBasicProgram y
printBasicProgram NOP = empty

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"

---

optimize :: [IR l -> IR l] -> IR l -> IR l
optimize fs ir =
  let ir' = foldr ($) ir fs
  in if ir == ir' then ir else optimize fs ir'

opt1 :: IR l -> IR l
opt1 (SEQ (DEFLOOP wVs _ rVs p) (SEQ (PRINT (algebra -> (f,c))) p')) = SEQ (DEFLOOP wVs [c] rVs (changeToFold f p)) (SEQ (PRINT (Leaf $ tupelize rVs)) p')
opt1 (SEQ x y) = opt1 x `SEQ` opt1 y
opt1 (IF c t e) = IF c (opt1 t) (opt1 e)
opt1 x = x

opt2 :: IR l -> IR l
opt2 (SEQ (DEFLOOP wVs ps [rV] p) (SEQ (PRINT (Leaf rV')) p')) | rV == rV' =  SEQ (DEFLOOP wVs ps [] (changeToPrintAcc p)) p'
opt2 (SEQ x y) = opt2 x `SEQ` opt2 y
opt2 (IF c t e) = IF c (opt2 t) (opt2 e)
opt2 x = x

opt3 :: IR l -> IR l
opt3 (SEQ (SEQ xx (UPDATE xk f xi v)) (SEQ (CALLLOOP [xk']) yy)) | xk == xk' = SEQ xx (SEQ (CALLLOOP [printAST $ f xi v]) yy)
opt3 (SEQ x y) = opt3 x `SEQ` opt3 y
opt3 (IF c t e) = IF c (opt3 t) (opt3 e)
opt3 (DEFLOOP wVs ps rVs p) = DEFLOOP wVs ps rVs (opt3 p)
opt3 x = x

changeToFold :: (String -> String -> AST) -> IR 'Body -> IR 'Body
changeToFold f (SEQ x y) = changeToFold f x `SEQ` changeToFold f y
changeToFold f (UPDATE xk _ xi v) = UPDATE xk f xi v
changeToFold f (IF c t e) = IF c (changeToFold f t) (changeToFold f e)
changeToFold _ x = x

changeToPrintAcc :: IR 'Body -> IR 'Body
changeToPrintAcc (SEQ x y) = changeToPrintAcc x `SEQ` changeToPrintAcc y
changeToPrintAcc (IF c t e) = IF c (changeToPrintAcc t) (changeToPrintAcc e)
changeToPrintAcc (RETURN [rV]) = PRINT $ Leaf rV
changeToPrintAcc x = x

-- mock implementation
algebra :: AST -> (String -> String -> AST, String)
algebra _ = (ast,"0") where
  ast a b = Infix (Leaf a) "+" (Leaf b)
