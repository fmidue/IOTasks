{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.Optimization where

import Test.IOTasks.Term
import Test.IOTasks.CodeGeneration.IR

import Test.IOTasks.Environment (Varname)

optExample = optimize [opt4,opt3,opt2,opt1]

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
    IR (DEFLOOP ident wVs (irmap (changeToFold f) p) : ENTERLOOP ident [Leaf c] rVs : PRINT (Leaf $ tupelize (map name rVs)) : p')
opt1 x (IR xs) = IR (x:xs)

-- move print inside a loop in case the loop-result is printed directly after the loop.
opt2 :: RewriteRule
opt2 (DEFLOOP ident wVs p) (IR (ENTERLOOP ident' ps [rV] : PRINT (Leaf rV') : p'))
  | name rV == rV' && ident == ident'
  = IR $ DEFLOOP ident wVs (irmap changeToPrintAcc p) : ENTERLOOP ident ps [] : p'
opt2 x (IR xs) = IR (x:xs)

-- merge update with following recursive call. (Current assumption: the value is only used in that call)
opt3 :: RewriteRule
opt3 (UPDATE xk f xi v) (IR (RECCALL l [xk'] : p'))
  | name xk == printAST xk'
  = IR $ RECCALL l [f (name xi) (name v)] : p'
opt3 (DEFLOOP ident wVs p) (IR xs) = IR $ DEFLOOP ident wVs (irfoldSpine opt3 irNOP p) : xs
opt3 (IF c t e) (IR xs) = IR $ IF c (irfoldSpine opt3 irNOP t) (irfoldSpine opt3 irNOP e) : xs
opt3 x (IR xs) = IR (x:xs)

-- merge updates
-- we know that once a variable is updated the previous version is not used anymore and can be deleted
opt4 :: RewriteRule
opt4 (UPDATE x1 f1 y1 v1) (IR (UPDATE x2 f2 y2 v2 : p'))
  | y2 == x1 = IR $ UPDATE x2 (\_ v -> f2 (printAST (f1 (name y1) (name v1))) v) y2 v2 : p'
opt4 x (IR xs) = IR (x:xs)

changeToFold :: (String -> String -> AST) -> IRTag -> IRTag
changeToFold f (UPDATE xk _ xi v) = UPDATE xk f xi v
changeToFold _ x = x

changeToPrintAcc :: IRTag -> IRTag
changeToPrintAcc (RETURN [rV]) = PRINT $ Leaf (name rV)
changeToPrintAcc x = x

-- extractAlgebra t tries to extracts the algebra from an AST
-- i.e. the parameters for expressing the represented term as a fold
-- incomplete implementation. Currently not sure how to write this in a general way.
-- especially the starting value is not correct in a general setting.
extractAlgebra :: AST -> Maybe (String -> String -> AST, String)
extractAlgebra (Node "sum" [_]) = Just (\a b -> Infix (Leaf a) "+" (Leaf b),"0")
extractAlgebra (Node "length" [Leaf _]) = Just (\_ b -> Infix (Leaf "1") "+" (Leaf b),"0")
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

reorderIR :: IR -> IR
reorderIR (IR xs) = IR $ reorder canSwapTags xs

-- assumes no shadowing of names
canSwapTags :: IRTag -> IRTag -> Bool
canSwapTags _ IF{} = False -- since this reverses the normalization of specifications
canSwapTags _ RECCALL{} = False -- we always have tail-recursion
canSwapTags READ{} _ = False
canSwapTags UPDATE{} (READ _) = True
canSwapTags (UPDATE x1 _ _ _) (UPDATE x2 _ y2 _) = x1 /= y2 && x1 > x2
canSwapTags (UPDATE x _ _ _) (PRINT t) = name x `notElem` leafs t
canSwapTags (UPDATE x _ _ _) (DEFLOOP _ _ p) = name x `notElem` usedVars p -- correctness here relies on the assumption that variable names in lambda functions do not clash with regular variable names
canSwapTags (UPDATE x _ _ _) (ENTERLOOP _ ps _) = name x `notElem` concatMap leafs ps
canSwapTags UPDATE{} RETURN{} = False
canSwapTags _ _ = False
-- canSwapTags (PRINT x) x2 = _
-- canSwapTags (IF c t e) x2 = _
-- canSwapTags (DEFLOOP l wVs p) x2 = _
-- canSwapTags (ENTERLOOP l ps rVs) x2 = _
-- canSwapTags (RECCALL l ps) x2 = _
-- canSwapTags (RETURN rVs) x2 = _

baseName :: IndexedVar -> Varname
baseName (IVar (x,_)) = x

usedVars :: IR -> [Varname]
usedVars (IR xs) = foldr used [] xs where
  used :: IRTag -> [Varname] -> [Varname]
  used (READ _) vs = vs
  used (UPDATE _ _ y v) vs = name y : name v : vs
  used (PRINT x) vs = leafs x ++ vs
  used (IF c t e) vs = leafs c ++ usedVars t ++ usedVars e ++ vs
  used (DEFLOOP _ wVs p) vs = usedVars p ++ vs
  used (ENTERLOOP l ps rVs) vs = name l : concatMap leafs ps ++ vs
  used (RECCALL l ps) vs = name l : concatMap leafs ps ++ vs
  used (RETURN x) vs = map name x ++ vs

bubbleSort :: Ord a => [a] -> [a]
bubbleSort = reorder (>)
