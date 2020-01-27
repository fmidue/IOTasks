{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.IR where

import Test.IOTasks.Environment
import Test.IOTasks.Term

import Text.PrettyPrint.HughesPJClass
import           Data.List                      ( intercalate )

data Level = Top | Body

data IR l where
  READ :: Varname -> IR l
  UPDATE :: Varname -> Varname -> IR l
  PRINT :: AST -> IR l
  IF :: AST -> IR l -> IR l -> IR l
  DEFLOOP :: [Varname] -> [Varname] -> [Varname] -> IR 'Body -> IR l
  CALLLOOP :: [Varname] -> IR 'Body
  RETURN :: [Varname] -> IR 'Body
  SEQ :: IR l -> IR l -> IR l
  NOP :: IR l

instance Pretty (IR l) where
  pPrint (READ x) = text $ x ++ " <- readLn"
  pPrint (UPDATE xi xk) = text $ "let " ++ xi ++ " = " ++ xk ++ " ++ " ++ "[v]"
  pPrint (PRINT ast) = text $ "print (" ++ flattenAST ast ++ ")"
  pPrint (IF ast t e) =
    hang (text $ "if " ++ flattenAST ast) 2 $
       hang (text "then do") 2 (pPrint t)
    $$ hang (text "else do") 2 (pPrint e)
  pPrint (DEFLOOP writeVars params returnVars p) =
    hang (text $ "let loop " ++ unwords writeVars ++ " = do") 6
    (pPrint p)
    $$ text (tupelize returnVars) <+> text ("<- loop " ++ unwords params)
  pPrint (CALLLOOP xs) = text "loop" <+> text (tupelize xs)
  pPrint (RETURN returnVars) = text "return" <+> text (tupelize returnVars)
  pPrint (SEQ x y) = pPrint x $$ pPrint y
  pPrint NOP = empty

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"

flattenAST :: AST -> String
flattenAST = go "" "" where
  go l r (Node s ts) = l ++ s ++ " " ++ unwords (map (go "(" ")") ts) ++ r
  go l r (Infix x op y) = l ++ go "(" ")" x ++ " " ++ op ++  " " ++  go "(" ")" y ++ r
  go _ _ (Leaf s) = s

data FoldT a = FoldT (Int -> a -> a) a

evalFoldT :: FoldT a -> [Int] -> a
evalFoldT (FoldT f c) = foldr f c
