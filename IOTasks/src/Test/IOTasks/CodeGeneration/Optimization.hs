{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Test.IOTasks.Term
import Test.IOTasks.CodeGeneration.IRGraph

import Test.IOTasks.Environment (Varname)

optExample = optimize [opt2,opt1]

optimize :: [[Def] -> SpineFold IRSpine] -> IRSpine -> IRSpine
optimize fs ir =
  let ir' = foldr foldrSpine ir fs
  in if ir == ir' then ir else optimize fs ir'

idFold :: [Def] -> SpineFold IRSpine
idFold ds =
  let
    fRead v (IRSpine s ds') = IRSpine (READ v s) ds'
    fPrint ast (IRSpine s ds') = IRSpine (PRINT ast s) ds'
    fEnterLoop l ps rVs (IRSpine s ds') = IRSpine (ENTERLOOP l ps rVs s) ds'
    fIf c (IRSpine s1 ds') (IRSpine s2 _) = IRSpine (IF c s1 s2) ds'
    fReturn rVs = IRSpine (RETURN rVs) ds
    fRecCall l ps = IRSpine (RECCALL l ps) ds
    fNop = IRSpine NOP ds
  in SpineFold{..}

-- replace list updates with an accumulating parameter based on the fact that the acumulated list is used with a fold.
-- Assumption: the result of the loop is only used once.
opt1 :: [Def] -> SpineFold IRSpine
opt1 ds =
  let
    enterLoop l _ rVs (IRSpine (PRINT (extractAlgebra -> Just (f,c)) p') ds') =
      IRSpine
        (ENTERLOOP l [Literal c] rVs $ PRINT (Literal . tupelize $ map name rVs) p')
        (updateDef l (changeToFold f) ds')
    enterLoop l ps rVs b = fEnterLoop (idFold ds) l ps rVs b
  in (idFold ds){fEnterLoop = enterLoop}

-- move print inside a loop in case the loop-result is printed directly after the loop.
opt2 :: [Def] -> SpineFold IRSpine
opt2 ds =
  let
    enterLoop l ps [rV] (IRSpine (PRINT (Var rV') p') ds')
      | rV == rV' = IRSpine (ENTERLOOP l ps [] p') (updateDef l changeToPrintAcc ds')
    enterLoop l ps rVs b = fEnterLoop (idFold ds) l ps rVs b
  in (idFold ds){fEnterLoop = enterLoop}

-- merge update with following recursive call. (Current assumption: the value is only used in that call)
-- opt3 :: SpineFold
-- opt3 (UPDATE xk f xi v) (IR (RECCALL l [xk'] : p'))
--   | name xk == printAST xk'
--   = IR $ RECCALL l [f (name xi) (name v)] : p'
-- opt3 (DEFLOOP ident wVs p) (IR xs) = IR $ DEFLOOP ident wVs (irfoldSpine opt3 irNOP p) : xs
-- opt3 (IF c t e) (IR xs) = IR $ IF c (irfoldSpine opt3 irNOP t) (irfoldSpine opt3 irNOP e) : xs
-- opt3 x (IR xs) = IR (x:xs)

-- merge updates
-- we know that once a variable is updated the previous version is not used anymore and can be deleted
-- opt4 :: SpineFold
-- opt4 (UPDATE x1 f1 y1 v1) (IR (UPDATE x2 f2 y2 v2 : p'))
--   | y2 == x1 = IR $ UPDATE x2 (\_ v -> f2 (printAST (f1 (name y1) (name v1))) v) y2 v2 : p'
-- opt4 x (IR xs) = IR (x:xs)
--
changeToFold :: (String -> String -> Expr) -> DefRhs -> DefRhs
changeToFold f (UPDATE _ xi v) = UPDATE f xi v
changeToFold f (DEFLOOP wVs (IRSpine s ds)) = DEFLOOP wVs $ IRSpine s (mapRhs (changeToFold f) ds)

changeToPrintAcc :: DefRhs -> DefRhs
changeToPrintAcc (DEFLOOP wVs ir) =
  let
  changeReturn [rV] = IRSpine (PRINT (Var rV) NOP) (defs ir)
  changeReturn rVs = fReturn (idFold (defs ir)) rVs
  in DEFLOOP wVs $ foldrSpine (\x -> (idFold x){fReturn = changeReturn}) ir
changeToPrintAcc x = x

-- extractAlgebra t tries to extracts the algebra from an AST
-- i.e. the parameters for expressing the represented term as a fold
-- incomplete implementation. Currently not sure how to write this in a general way.
-- especially the starting value is not correct in a general setting.
extractAlgebra :: Expr -> Maybe (String -> String -> Expr, String)
extractAlgebra (Node "sum" [_]) = Just (\a b -> Infix (Literal a) "+" (Literal b),"0")
extractAlgebra (Node "length" [Var _]) = Just (\_ b -> Infix (Literal "1") "+" (Literal b),"0")
extractAlgebra _ = Nothing

-- swapping elements based on some binary predicate (True ~= swap)
bubble :: (a -> a -> Bool) -> [a] -> [a]
bubble (~>) = foldr f [] where
  f x [] = [x]
  f x (y:ys)
    | x ~> y     = y : x : ys
    | otherwise = x : y : ys

reorder :: Eq a => (a -> a -> Bool) -> [a] -> [a]
reorder f xs =
  let xs' = bubble f xs
  in if xs == xs'
    then xs'
    else reorder f xs'

baseName :: IndexedVar -> Varname
baseName (IVar (x,_)) = x

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = reorder (>)
