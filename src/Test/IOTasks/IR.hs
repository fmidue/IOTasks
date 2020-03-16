{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Test.IOTasks.IR where

import Test.IOTasks.Environment
import Test.IOTasks.Term

import Text.PrettyPrint.HughesPJClass (Doc, Pretty)
import qualified Text.PrettyPrint.HughesPJClass as PP
import Data.List (intercalate)

import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.Tree.Pretty
import Data.Function (on)

import Data.Functor.Foldable

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

data IRF l x where
  READF :: Varname -> IRF l x
  UPDATEF :: Varname -> (Varname -> Varname -> AST) -> Varname -> Varname -> IRF l x -- UPDATE x f y v assigns the result f applied to y and v to x, ie. x := f y v
  PRINTF :: AST -> IRF l x
  IFF :: AST -> x -> x -> IRF l x
  DEFLOOPF :: [Varname] -> [Arg] -> [Varname] -> IR 'Body -> IRF l x -- define and call a loop
  CALLLOOPF :: [Varname] -> IRF 'Body x -- recursive call of the current loop
  RETURNF :: [Varname] -> IRF 'Body x -- finish current loop
  SEQF :: x -> x -> IRF l x
  NOPF :: IRF l x

instance Functor (IRF l) where
  fmap _ (READF x) = READF x
  fmap _ (UPDATEF x1 x2 x3 x4) = UPDATEF x1 x2 x3 x4
  fmap _ (PRINTF x) = PRINTF x
  fmap f (IFF x1 x2 x3) = IFF x1 (f x2) (f x3)
  fmap f (DEFLOOPF x1 x2 x3 x4) = DEFLOOPF x1 x2 x3 x4
  fmap _ (CALLLOOPF x) = CALLLOOPF x
  fmap _ (RETURNF x) = RETURNF x
  fmap f (SEQF x1 x2) = SEQF (f x1) (f x2)
  fmap _ NOPF = NOPF

type instance Base (IR l) = IRF l

instance Recursive (IR l) where
  project (READ x) = READF x
  project (UPDATE x1 x2 x3 x4) = UPDATEF x1 x2 x3 x4
  project (PRINT x) = PRINTF x
  project (IF x1 x2 x3) = IFF x1 x2 x3
  project (DEFLOOP x1 x2 x3 x4) = DEFLOOPF x1 x2 x3 x4
  project (CALLLOOP x) = CALLLOOPF x
  project (RETURN x) = RETURNF x
  project (SEQ x1 x2) = SEQF x1 x2
  project NOP = NOPF

instance Corecursive (IR l) where
  embed (READF x) = READ x
  embed (UPDATEF x1 x2 x3 x4) = UPDATE x1 x2 x3 x4
  embed (PRINTF x) = PRINT x
  embed (IFF x1 x2 x3) = IF x1 x2 x3
  embed (DEFLOOPF x1 x2 x3 x4) = DEFLOOP x1 x2 x3 x4
  embed (CALLLOOPF x) = CALLLOOP x
  embed (RETURNF x) = RETURN x
  embed (SEQF x1 x2) = SEQ x1 x2
  embed NOPF = NOP

deriving instance Show (IR l)

instance Eq (IR l) where
  (==) = (==) `on` show

type Algebra l = IRF l (IR l) -> IR l

topToBody :: IR 'Top -> IR 'Body
topToBody (READ x) = READ x
topToBody (UPDATE x1 x2 x3 x4) = UPDATE x1 x2 x3 x4
topToBody (PRINT x) = PRINT x
topToBody (IF x1 x2 x3) = IF x1 (topToBody x2) (topToBody x3)
topToBody (DEFLOOP x1 x2 x3 x4) = DEFLOOP x1 x2 x3 x4
topToBody (SEQ x1 x2) = SEQ (topToBody x1) (topToBody x2)
topToBody NOP = NOP

instance Pretty (IR l) where
  pPrint = PP.text . drawVerticalTree . toTree where
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
printBasicProgram (READ x) = PP.text $ x ++ " <- readLn"
printBasicProgram (UPDATE xi f xk v) = PP.text $ "let " ++ xi ++ " = " ++ printAST (f xk v)
printBasicProgram (PRINT ast) = PP.text $ "print " ++ formatParam CallParam [printAST ast]
printBasicProgram (IF ast t e) =
  PP.hang (PP.text $ "if " ++ printAST ast) 2 $
     PP.hang (PP.text "then do") 2 (printBasicProgram t)
  PP.$$ PP.hang (PP.text "else do") 2 (printBasicProgram e)
printBasicProgram (DEFLOOP writeVars params [] p) =
  PP.hang (PP.text $ "let loop " ++ formatParam FunctionParam writeVars ++ " = do") 6
  (printBasicProgram p)
  PP.$$ PP.text ("loop " ++ formatParam CallParam params)
printBasicProgram (DEFLOOP writeVars params returnVars p) =
  PP.hang (PP.text $ "let loop " ++ formatParam FunctionParam writeVars ++ " = do") 6
  (printBasicProgram p)
  PP.$$ PP.text (formatParam ReturnParam returnVars) PP.<+> PP.text ("<- loop " ++ formatParam CallParam params)
printBasicProgram (CALLLOOP xs) = PP.text "loop" PP.<+> PP.text (formatParam CallParam xs)
printBasicProgram (RETURN returnVars) = PP.text "return" PP.<+> PP.text (formatParam ReturnParam returnVars)
printBasicProgram (SEQ x y) = printBasicProgram x PP.$$ printBasicProgram y
printBasicProgram NOP = PP.empty

data Mode = FunctionParam | CallParam | ReturnParam

formatParam :: Mode -> [String] -> String
formatParam FunctionParam xs = unwords xs
formatParam CallParam xs = unwords $ map (\x -> if length (words x) > 1 then "("++ x ++")" else x) xs
formatParam ReturnParam xs = tupelize xs

tupelize :: [String] -> String
tupelize [] = ""
tupelize [v] = v
tupelize vs = "(" ++ intercalate "," vs ++ ")"

---
optExample = optimize [opt3,opt2,opt1]

optimize :: [Algebra l] -> IR l -> IR l
optimize fs ir =
  let ir' = foldr cata ir fs
  in if ir == ir' then ir else optimize fs ir'

-- replace list updates with an accumulating parameter based on the fact that the acumulated list is used with a fold.
-- Assumption: the result of the loop is only uesed once.
opt1 :: Algebra l
opt1 (SEQF (DEFLOOP wVs _ rVs p) (SEQ (PRINT (extractAlgebra -> Just (f,c))) p')) = SEQ (DEFLOOP wVs [c] rVs (cata (changeToFold f) p)) (SEQ (PRINT (Leaf $ tupelize rVs)) p')
opt1 x = embed x

-- move print inside a loop in case the loop-result is printed directly after the loop.
opt2 :: Algebra l
opt2 (SEQF (DEFLOOP wVs ps [rV] p) (SEQ (PRINT (Leaf rV')) p')) | rV == rV' =  SEQ (DEFLOOP wVs ps [] (cata changeToPrintAcc p)) p'
opt2 x = embed x

-- merge update with following recursive call. (Current assumption: the value is only used in that call)
opt3 :: Algebra l
opt3 (SEQF (SEQ xx (UPDATE xk f xi v)) (SEQ (CALLLOOP [xk']) yy)) | xk == xk' = SEQ xx (SEQ (CALLLOOP [printAST $ f xi v]) yy)
opt3 (DEFLOOPF wVs ps rVs p) = DEFLOOP wVs ps rVs (cata opt3 p)
opt3 x = embed x

changeToFold :: (String -> String -> AST) -> Algebra 'Body
changeToFold f (UPDATEF xk _ xi v) = UPDATE xk f xi v
changeToFold _ x = embed x

changeToPrintAcc :: Algebra 'Body
changeToPrintAcc (RETURNF [rV]) = PRINT $ Leaf rV
changeToPrintAcc x = embed x

-- extractAlgebra t tries to extracts the algebra from an AST
-- i.e. the parameters for expressing the represented term as a fold
-- incomplete implementation. Currently not sure how to write this in a general way.
extractAlgebra :: AST -> Maybe (String -> String -> AST, String)
extractAlgebra (Node "sum" [_]) = Just (\a b -> Infix (Leaf a) "+" (Leaf b),"0")
extractAlgebra (Node "length" [_]) = Just (\_ b -> Infix (Leaf "1") "+" (Leaf b),"0")
extractAlgebra _ = Nothing
