{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Test.IOTasks.Term
import Test.IOTasks.CodeGeneration.IR

optExample = optimize [opt3,opt2,opt1]

type RewriteRule = IRTag -> IR -> IR

optimize :: [RewriteRule] -> IR -> IR
optimize fs ir =
  let ir' = foldr (`irfoldSpine` mempty) ir fs
  in if ir == ir' then ir else optimize fs ir'

-- replace list updates with an accumulating parameter based on the fact that the acumulated list is used with a fold.
-- Assumption: the result of the loop is only used once.
opt1 :: RewriteRule
opt1 (DEFLOOP ident wVs p) (IR (ENTERLOOP ident' _ rVs: PRINT (extractAlgebra -> Just (f,c)) : p'))
  | ident == ident' =
    IR (DEFLOOP ident wVs (irmap (changeToFold f) p) : ENTERLOOP ident [c] rVs : PRINT (Leaf $ tupelize rVs) : p')
opt1 x (IR xs) = IR (x:xs)

-- move print inside a loop in case the loop-result is printed directly after the loop.
opt2 :: RewriteRule
opt2 (DEFLOOP ident wVs p) (IR (ENTERLOOP ident' ps [rV] : PRINT (Leaf rV') : p'))
  | rV == rV' && ident == ident'
  = IR $ DEFLOOP ident wVs (irmap changeToPrintAcc p) : ENTERLOOP ident ps [] : p'
opt2 x (IR xs) = IR (x:xs)

-- merge update with following recursive call. (Current assumption: the value is only used in that call)
opt3 :: RewriteRule
opt3 (UPDATE xk f xi v) (IR (RECCALL [xk'] : p'))
  | xk == xk'
  = IR $ RECCALL [printAST $ f xi v] : p'
opt3 (DEFLOOP ident wVs p) (IR xs) = IR $ DEFLOOP ident wVs (irfoldSpine opt3 irNOP p) : xs
opt3 (IF c t e) (IR xs) = IR $ IF c (irfoldSpine opt3 irNOP t) (irfoldSpine opt3 irNOP e) : xs
opt3 x (IR xs) = IR (x:xs)

changeToFold :: (String -> String -> AST) -> IRTag -> IRTag
changeToFold f (UPDATE xk _ xi v) = UPDATE xk f xi v
changeToFold _ x = x

changeToPrintAcc :: IRTag -> IRTag
changeToPrintAcc (RETURN [rV]) = PRINT $ Leaf rV
changeToPrintAcc x = x

-- extractAlgebra t tries to extracts the algebra from an AST
-- i.e. the parameters for expressing the represented term as a fold
-- incomplete implementation. Currently not sure how to write this in a general way.
extractAlgebra :: AST -> Maybe (String -> String -> AST, String)
extractAlgebra (Node "sum" [_]) = Just (\a b -> Infix (Leaf a) "+" (Leaf b),"0")
extractAlgebra (Node "length" [_]) = Just (\_ b -> Infix (Leaf "1") "+" (Leaf b),"0")
extractAlgebra _ = Nothing
