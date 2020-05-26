{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.IR where

import Control.Applicative ((<|>))

import Data.Maybe (fromMaybe)
import Data.List (sort, group, intercalate, nub)

import Data.Environment
import Data.Term.AST

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP

import Test.IOTasks.CodeGeneration.FreshVar

data Instruction
  = READ Var
  | PRINT Var
  | IF Var [Instruction] [Instruction]
  | TAILCALL Var [Var] -- TAILCALL f [x1,..,xn] => f x1 .. xn
  | BINDCALL Var [Var] [Var] -- BINDCALL f [x1,..,xn] [v1,..,vm] => (v1,..,vm) <- f x1 .. xn
  | YIELD [Var]
  | NOP
  deriving (Show,Eq)

type Var = IndexedVar

type AM = ([Instruction],([Def],[F]),Vals,[[Var]],[Input],[Output])

type IRProgram = ([Instruction], [Def], [F])
type Vals = Environment Var
type Def = (Var,DefRhs,Int,Varname)
data DefRhs where
  -- updates
  U :: (AST Var -> AST Var -> AST Var) -> Var -> Var -> DefRhs
  -- nested updates
  N :: (AST Var -> AST Var -> AST Var) -> DefRhs -> Var -> DefRhs
  Const :: AST Var -> DefRhs
type F = (Var, ([Var],[Var]), [Instruction]) -- the two sets of are the 'original' parameters and the parameters introduced by transformations
type Input = Int
data Output = I Int | O Int deriving (Show,Eq) -- simple trace elements

-- very basic sequencing, assumes well-formed input, i.e. not duplicate varaiable definitions etc.
(<:>) :: IRProgram -> IRProgram -> IRProgram
(is1,ds1,fs1) <:> (is2,ds2,fs2) =
  let uc1 = instructionVars is1
      uc2 = instructionVars is2
      ds1' = updateUseCount uc2 ds1
      ds2' = updateUseCount uc1 ds2
      ds' = updateUseCount (usedVars ds2') ds1' ++ updateUseCount (usedVars ds1') ds2'
  in (is1 ++ is2,ds',fs1 ++ fs2)

readIR :: Var -> IRProgram
readIR x = ([READ x],[],[])

initialValueIR :: Var -> Var -> Var -> Varname -> IRProgram
initialValueIR x y v scp = ([],[(x,U (\_ v -> App (PostApp v (Leaf ":")) (Leaf "[]")) y v,0,scp)],[])

updateIR :: Var -> Var -> Var -> Varname -> IRProgram
updateIR x y v scp = ([],[(x,U (\y v -> App (PostApp y (Leaf "++")) (App (PostApp v (Leaf ":")) (Leaf "[]"))) y v,0,scp)],[])

printIR :: Var -> IRProgram
printIR x = ([PRINT x],[],[])

valueDefIR :: Var -> AST Var -> Varname -> IRProgram
valueDefIR x t scp = ([],[(x,Const t,0,scp)],[])

ifIR :: Var -> IRProgram -> IRProgram -> IRProgram
ifIR c (is1,ds1,fs1) (is2,ds2,fs2) = ([IF c is1 is2],ds1 ++ ds2,fs1 ++ fs2)

defLoopIR :: Var -> [Var] -> IRProgram -> IRProgram
defLoopIR f xs (b,ds,fs) = ([],ds,(f,(xs,[]),b):fs)

enterLoopIR :: Var -> [Var] -> [Var] -> IRProgram
enterLoopIR f ps rvs = ([BINDCALL f ps rvs],[],[])

recCallIR :: Var -> [Var] -> IRProgram
recCallIR f ps = ([TAILCALL f ps],[],[])

yieldIR :: [Var] -> IRProgram
yieldIR rvs = ([YIELD rvs],[],[])

nopIR :: IRProgram
nopIR = ([NOP],[],[])

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
printInstruction (READ x) = PP.text $ "READ " ++ name x
printInstruction (PRINT t) = PP.text $ "PRINT " ++ name t
printInstruction (IF c t e) =
  PP.hang (PP.text ("IF " ++ name c)) 2
    ( PP.hang (PP.text "THEN") 2 (PP.vcat (map printInstruction t))
    PP.$$ PP.hang (PP.text "ELSE") 2 (PP.vcat (map printInstruction e))
    )
printInstruction (TAILCALL f ps) = PP.text ("TAILCALL " ++ name f) PP.<+> tupelize ps
printInstruction (BINDCALL f ps rvs) = PP.text ("BINDCALL " ++ name f) PP.<+> tupelize ps PP.<+> tupelize rvs
printInstruction (YIELD rvs) = PP.text "RETURN " PP.<+> tupelize rvs
printInstruction NOP = mempty

instructionVars :: [Instruction] -> [(Var,Int)]
instructionVars = map (\x -> (head x, length x)) . group . sort . concatMap go where
  go :: Instruction -> [Var]
  go (READ x) = [x]
  go (PRINT x) = [x]
  go (IF c t e) = c : concatMap go t ++ concatMap go e
  go (TAILCALL _ ps) = ps
  go (BINDCALL _ ps _) = ps
  go (YIELD xs) = xs
  go NOP = []

printDef :: Def -> Doc
printDef (x,rhs,_,_) = PP.text (name x ++ " :=") PP.<+> printDefRhs rhs

printDefRhs :: DefRhs -> Doc
printDefRhs (U f y v) = PP.text $ printFlat $ f (Leaf $ name y) (Leaf $ name v)
printDefRhs (N f y v) = PP.text $ printFlat $ f (toAST y) (Leaf $ name v)
printDefRhs (Const t) = PP.text $ printFlat t

printF :: F -> Doc
printF (f,(xs,ys),b) = PP.hang (PP.text (name f) PP.<+> tupelize (xs++ys) PP.<+> PP.text ":=") 2 $
  PP.vcat (map printInstruction b)

tupelize :: [Var] -> Doc
tupelize [] = PP.text ""
tupelize [v] = PP.text $ name v
tupelize vs = PP.text $ "(" ++ intercalate "," (map name vs) ++ ")"

-- manipulation of DefRhs
printDefTree :: DefRhs -> String
printDefTree = pPrintTree . toAST

toAST :: DefRhs -> AST Var
toAST (U f y v) = f (Var y) (Var v)
toAST (N f y v) = f (toAST y) (Var v)
toAST (Const t) = t

instance Show DefRhs where
  show (U f y v) = "U ? " ++ name y ++ " " ++ name v
  show (N f y v) = "N ? " ++ name v
  show (Const t) = "Const ?"

addDef :: (Var,DefRhs,Maybe Int,Varname) -> [Def] -> [Def]
addDef (x,d,mn,scp) ds =
  let n = fromMaybe 0 $ mn <|> lookup x (usedVars ds)
  in (x,d,n,scp) : updateUseCount (usedVars' [d]) ds

updateUseCount :: [(Var,Int)] -> [Def] -> [Def]
updateUseCount cs = foldr phi [] where
  phi (x,d,n,scp) ds = case lookup x cs of
    Just m -> (x,d,n+m,scp) : ds
    Nothing -> (x,d,n,scp) : ds

allVars :: [Def] -> [Var]
allVars ds = nub $ definedVars ds ++ map fst (usedVars ds)

allVarsInScope :: Varname -> [Def] -> [Var]
allVarsInScope scp ds = allVars $ filter (\(_,_,_,scp') -> scp == scp' ) ds

definedVars :: [Def] -> [Var]
definedVars = map (\(x,_,_,_) -> x)

usedVars :: [Def] -> [(Var,Int)]
usedVars = usedVars' . map (\(_,d,_,_) -> d)

usedVars' :: [DefRhs] -> [(Var,Int)]
usedVars' = map (\x -> (head x, length x)) . group . sort . concatMap go where
  go :: DefRhs -> [Var]
  go (U _ y v) = [y,v]
  go (N _ y v) = v : go y
  go (Const t) = vars t

lookup2 :: Eq k => k -> [(k,a,b)] -> Maybe (a,b)
lookup2 x ds = lookup x (map (\(x,d,n) -> (x,(d,n))) ds)

lookup3 :: Eq k => k -> [(k,a,b,c)] -> Maybe (a,b,c)
lookup3 x ds = lookup x (map (\(x,d,n,scp) -> (x,(d,n,scp))) ds)

lookupDef :: Var -> [Def] -> Maybe (DefRhs,Int,Varname)
lookupDef = lookup3

updateDef :: (Var,DefRhs) -> [Def] -> [Def]
updateDef (x,newRhs) = (\(ds',vs) -> updateUseCount vs ds') . updateDef' ([],[]) where
  updateDef' (ds',vs) [] = (ds',vs)
  updateDef' (ds',vs) ((y,oldRhs,n,scp):ds)
    | x == y =
      let
        newUsed = usedVars' [newRhs]
        oldUsed = usedVars' [oldRhs]
        diff = usageDiff newUsed oldUsed
      in (ds'++(x,newRhs,n,scp):ds,diff)
    | otherwise = updateDef' ((y,oldRhs,n,scp):ds',vs) ds

-- 1st arg = new, 2nd arg = old
-- assumes ordered lists (like the ones produced by usedVars)
usageDiff :: [(Var,Int)] -> [(Var,Int)] -> [(Var,Int)]
usageDiff [] [] = []
usageDiff xs [] = xs
usageDiff [] ys = map (\(y,m) -> (y, negate m)) ys
usageDiff ((x,n):xs) ((y,m):ys)
  | x == y = (x,n-m) : usageDiff xs ys
  | x > y = (y,negate m) : usageDiff ((x,n):xs) ys
  | otherwise = (x,n) : usageDiff xs ((y,m):ys)

lookupF :: Var -> [F] -> Maybe (([Var],[Var]),[Instruction])
lookupF = lookup2

updateF :: Var -> (F -> F) -> [F] -> [F]
updateF _ _ [] = []
updateF f df ((g,ps,bg):fs)
  | f == g = df (f,ps,bg) : fs
  | otherwise = (g,ps,bg) : updateF f df fs
