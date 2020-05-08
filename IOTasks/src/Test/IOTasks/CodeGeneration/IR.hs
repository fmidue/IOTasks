{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.CodeGeneration.IR where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (sort, group, intercalate)
import Data.Proxy

import Data.Environment
import Data.Term
import Data.Term.Liftable
import qualified Data.Term.Liftable.Prelude as T
import Data.Term.AST

import Text.PrettyPrint.HughesPJClass (Doc)
import qualified Text.PrettyPrint.HughesPJClass as PP

import Debug.Trace

data Instruction
  = READ Var
  | PRINT Var
  | IF Var [Instruction] [Instruction]
  | TAILCALL Var [Var] -- TAILCALL f [x1,..,xn] => f x1 .. xn
  | BINDCALL Var [Var] [Var] -- BINDCALL f [x1,..,xn] [v1,..,vm] => (v1,..,vm) <- f x1 .. xn
  | YIELD [Var]
  | NOP
  deriving (Show,Eq)

type Var = String

type AM = ([Instruction],([Def],[F]),Vals,[[Var]],[Input],[Output])

type IRProgram = ([Instruction], [Def], [F])
type Vals = Environment Var
type Def = (Var,DefRhs,Int)
data DefRhs where
  U :: (AST Var -> AST Var -> AST Var) -> Var -> Var -> DefRhs
  N1L :: (AST Var -> AST Var -> AST Var) -> DefRhs -> Var -> DefRhs
  N1R :: (AST Var -> AST Var -> AST Var) -> Var -> DefRhs -> DefRhs
  N2  :: (AST Var -> AST Var -> AST Var) -> DefRhs -> DefRhs -> DefRhs
  Const :: AST Var -> DefRhs
type F = (Var, [Var], [Instruction])
type Input = Int
data Output = I Int | O Int deriving (Show,Eq) -- simple trace elements

-- very basic sequencing, assumes well-formed input, i.e. not duplicate varaiable definitions etc.
(<:>) :: IRProgram -> IRProgram -> IRProgram
(is1,ds1,fs1) <:> (is2,ds2,fs2) = (is1 ++ is2,foldr (\(x,d,_) -> addDef (x,d)) ds1 ds2,fs1 ++ fs2)

readIR :: Var -> IRProgram
readIR x = ([READ x],[],[])

initialValueIR :: Var -> Var -> Var -> IRProgram
initialValueIR x y v = ([],[(x,U (\_ v -> App (PostApp v (Leaf ":")) (Leaf "[]")) y v,0)],[])

updateIR :: Var -> Var -> Var -> IRProgram
updateIR x y v = ([],[(x,U (\y v -> App (PostApp y (Leaf "++")) (App (PostApp v (Leaf ":")) (Leaf "[]"))) y v,0)],[])

printIR :: Var -> IRProgram
printIR x = ([PRINT x],[],[])

valueDefIR :: Var -> AST Var -> IRProgram
valueDefIR x t = ([],[(x,Const t,0)],[])

ifIR :: Var -> IRProgram -> IRProgram -> IRProgram
ifIR c (is1,ds1,fs1) (is2,ds2,fs2) = ([IF c is1 is2],ds1 ++ ds2,fs1 ++ fs2)

defLoopIR :: Var -> [Var] -> IRProgram -> IRProgram
defLoopIR f xs (b,ds,fs) = ([],ds,(f,xs,b):fs)

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
printInstruction (READ x) = PP.text $ "READ " ++ x
printInstruction (PRINT t) = PP.text $ "PRINT " ++ t
printInstruction (IF c t e) =
  PP.hang (PP.text ("IF " ++ c)) 2
    ( PP.hang (PP.text "THEN") 2 (PP.vcat (map printInstruction t))
    PP.$$ PP.hang (PP.text "ELSE") 2 (PP.vcat (map printInstruction e))
    )
printInstruction (TAILCALL f ps) = PP.text ("TAILCALL " ++ f) PP.<+> tupelize ps
printInstruction (BINDCALL f ps rvs) = PP.text ("BINDCALL " ++ f) PP.<+> tupelize ps PP.<+> tupelize rvs
printInstruction (YIELD rvs) = PP.text "RETURN " PP.<+> tupelize rvs
printInstruction NOP = mempty

printDef :: Def -> Doc
printDef (x,rhs,_) = PP.text (x ++ " :=") PP.<+> printDefRhs rhs

printDefRhs :: DefRhs -> Doc
printDefRhs (U f y v) = PP.text $ printFlat' $ f (Leaf y) (Leaf v)
printDefRhs (N1L f y v) = PP.text $ printFlat' $ f (toAST y) (Leaf v)
printDefRhs (N1R f y v) = PP.text $ printFlat' $ f (Leaf y) (toAST v)
printDefRhs (N2 f y v) = PP.text $ printFlat' $ f (toAST y) (toAST v)
printDefRhs (Const t) = PP.text $ printFlat' t

printF :: F -> Doc
printF (f,xs,b) = PP.hang (PP.text f PP.<+> tupelize xs PP.<+> PP.text ":=") 2 $
  PP.vcat (map printInstruction b)

tupelize :: [String] -> Doc
tupelize [] = PP.text ""
tupelize [v] = PP.text v
tupelize vs = PP.text $ "(" ++ intercalate "," vs ++ ")"

-- manipulation of DefRhs
printDefTree :: DefRhs -> String
printDefTree = pPrintTree . toAST

toAST :: DefRhs -> AST Var
toAST (U f y v) = f (Leaf y) (Leaf v)
toAST (N1L f y v) = f (toAST y) (Leaf v)
toAST (N1R f y v) = f (Leaf y) (toAST v)
toAST (N2 f y v) = f (toAST y) (toAST v)
toAST (Const t) = t

instance Show DefRhs where
  show (U f y v) = "U ? " ++ y ++ " " ++ v
  show (N1L f y v) = "N1L ? " ++ v
  show (N1R f y v) = "N1R ? " ++ y
  show (N2 f y v) = "N2 ?"
  show (Const t) = "Const ?"

addDef :: (Var,DefRhs) -> [Def] -> [Def]
addDef (x,d) ds =
  let n = fromMaybe 0 $ lookup x $ usedVars (map (\(_,d,_) -> d) ds)
  in (x,d,n) : updateUseCount (usedVars [d]) ds

updateUseCount :: [(Var,Int)] -> [Def] -> [Def]
updateUseCount cs = foldr phi [] where
  phi (x,d,n) ds = case lookup x cs of
    Just m -> (x,d,n+m) : ds
    Nothing -> (x,d,n) : ds

definedVars :: [Def] -> [Var]
definedVars = map (\(x,_,_) -> x)

usedVars :: [DefRhs] -> [(Var,Int)]
usedVars = map (\x -> (head x, length x)) . group . sort . concatMap go where
  go :: DefRhs -> [Var]
  go (U _ y v) = [y,v]
  go (N1L _ y v) = v : go y
  go (N1R _ y v) = y : go v
  go (N2 _ y v) = go y ++ go v
  go (Const t) = vars t

lookup2 :: Eq k => k -> [(k,a,b)] -> Maybe (a,b)
lookup2 x ds = lookup x (map (\(x,d,n) -> (x,(d,n))) ds)

lookupDef :: Var -> [Def] -> Maybe (DefRhs,Int)
lookupDef = lookup2

updateDef :: (Var,DefRhs) -> [Def] -> [Def]
updateDef (x,newRhs) = (\(ds',vs) -> updateUseCount vs ds') . updateDef' ([],[]) where
  updateDef' (ds',vs) [] = (ds',vs)
  updateDef' (ds',vs) ((y,oldRhs,n):ds)
    | x == y =
      let
        newUsed = usedVars [newRhs]
        oldUsed = usedVars [oldRhs]
        diff = usageDiff newUsed oldUsed
      in (ds'++(x,newRhs,n):ds,diff)
    | otherwise = updateDef' ((y,oldRhs,n):ds',vs) ds

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

lookupF :: Var -> [F] -> Maybe ([Var],[Instruction])
lookupF = lookup2

updateF :: Var -> (F -> F) -> [F] -> [F]
updateF _ _ [] = []
updateF f df ((g,ps,bg):fs)
  | f == g = df (f,ps,bg) : fs
  | otherwise = (g,ps,bg) : updateF f df fs
