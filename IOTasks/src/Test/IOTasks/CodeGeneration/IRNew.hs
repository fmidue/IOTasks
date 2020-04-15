{-# LANGUAGE RankNTypes #-}
module Test.IOTasks.CodeGeneration.IRNew where

import Data.Maybe (fromJust)

import Test.IOTasks.Term
import Test.IOTasks.Term.ITerm

data Instruction
  = READ Var
  | PRINT Var
  | IF Var [Instruction] [Instruction]
  | TAILCALL Var [Var] -- TAILCALL f [x1,..,xn] => f x1 .. xn
  | BINDCALL Var [Var] [Var] -- BINDCALL f [x1,..,xn] [v1,..,vm] => (v1,..,vm) <- f x1 .. xn
  | RETURN [Var]
  | NOP
  deriving (Show,Eq)

type Var = String

type AM = ([Instruction],[Def],[Value],[[Var]],[Input],[Output])

type Value = (Var,Payload)
type Def = (Var,DefRhs)
data DefRhs = IntValueDef (ITerm Var Int) | BoolValueDef (ITerm Var Bool) | LoopDef [Var] [Instruction]
data Payload = I Int | B Bool deriving (Show,Eq)
type Input = Int
data Output = R Int | W Int deriving (Show,Eq) -- simple trace elements

runInstructions :: [Instruction] -> [Def] -> [Input] -> [Output]
runInstructions p d i = let (_,_,_,_,_,o) = runAM (p,d,[],[],i,[]) in o

runAM :: AM -> AM
runAM ([],d,vs,rvs,i,o) = ([],d,vs,rvs,i,o)
runAM m = runAM $ stepAM m

stepAM :: AM -> AM
stepAM ([],_,_,_,_,_) = error "no instruction to execute"
stepAM (READ _:_,_,_,_,[],_) = error "not enough inputs"
stepAM (READ x:p',d,vs,rvs,i:is,o) = (p',d,updateValue (x,I i) vs,rvs,is,R i:o)
stepAM (PRINT x:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,W (intValue x d vs):o)
stepAM (IF x t e:p',d,vs,rvs,i,o)
  | boolValue x d vs = (t++p',d,vs,rvs,i,o)
  | otherwise = (e++p',d,vs,rvs,i,o)
stepAM (TAILCALL l ps:p',d,vs,rvs,i,o) = (getInstructions l d ++ p',d,setParameters l ps d vs,rvs,i,o)
stepAM (BINDCALL l ps bs:p',d,vs,rvs,i,o) = (getInstructions l d ++ p',d,setParameters l ps d vs,bs:rvs,i,o)
stepAM (RETURN _:_,_,_,[],_,_) = error "missing binders for returned values"
stepAM (RETURN xs:p',d,vs,rvs:rvss,i,o)
  | length vs == length rvs = (p', d,updateValues (zipWith (\x rv -> (rv,getPayload x vs)) xs rvs) vs,rvss,i,o)
  | otherwise = error "wrong number of binders for returned values"
stepAM (NOP:p',d,vs,rvs,i,o) = (p',d,vs,rvs,i,o)

setParameters :: Var -> [Var] -> [Def] -> [Value] -> [Value]
setParameters l xs ds vs = updateValues (zip xs (map (`getPayload` vs) (getParamNames l ds))) vs

updateValues :: [Value] -> [Value] -> [Value]
updateValues xs ys = foldr updateValue ys xs

updateValue :: Value -> [Value] -> [Value]
updateValue = (:) -- inefficient but should work

getPayload :: Var -> [Value] -> Payload
getPayload x = fromJust . lookup x

intValue :: Var -> [Def] -> [Value] -> Int
intValue x ds vs = case lookup x vs of
  Just (I i) -> i
  Nothing -> case lookup x ds of
    Just (IntValueDef t) -> _ $ foldr (\x vs' -> intValue x ds vs) vs (termVars t)
    _ -> error ""
  _ -> error "not an Int value"

boolValue :: Var -> [Def] -> [Value] -> Bool
boolValue x ds vs = case lookup x vs of
  Just (B b) -> b
  _ -> error "not a Bool value"

getInstructions :: Var -> [Def] -> [Instruction]
getInstructions x ds = case fromJust $ lookup x ds of
  LoopDef _ p -> p
  _ -> error "variable is not a loop name"

getParamNames :: Var -> [Def] -> [Var]
getParamNames x ds = case fromJust $ lookup x ds of
  LoopDef xs _ -> xs
  _ -> error "variable is not a loop name"
