{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.IOTasks.CodeGeneration.Translation where

import Test.IOTasks.Specification
import Test.IOTasks.CodeGeneration.IR
import Test.IOTasks.CodeGeneration.Analysis
import Test.IOTasks.CodeGeneration.FreshVar

import Control.Monad.State

import Data.Term
import Data.Term.AST

import qualified Data.Map as Map

programIR :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> IRProgram
programIR s = fst $ runFreshVarM (programIR' s) initState

programIR' :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> FreshVarM IRProgram
programIR' s = foldr1 (<:>) <$> mapM (translate (rootUsageFacts x)) x
  where x = analyse s

translate :: (SynTerm t (AST Varname)) => Facts Usage -> AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
translate fs (AnnAction _ (ReadInput x _)) =
  case Map.lookup x fs of
    Just Current ->
      return $ readIR (Plain x)
    Just All -> do
      v <- freshName "v"
      xk <- currentName x
      xi <- freshName x
      return $ case xk of
        Initial _ -> readIR v <:> initialValueIR xi xk v
        _ -> readIR v <:> updateIR xi xk v
    Nothing -> error "invalid spec"
translate _ (AnnAction _ (WriteOutput True _ _)) = return nopIR
translate fs (AnnAction _ (WriteOutput False _ (t:_))) = do
  ast <- adjustVars fs $ viewTerm t
  x <- freshName "t" -- TODO: find way to make this generic
  return $ printIR x <:> valueDefIR x ast
translate _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate fs (AnnAction _ (Branch c as1 as2)) = do
  ast <- adjustVars fs $ viewTerm c
  var <- freshName "cond"
  x <- mapM (translate fs) as2
  y <- mapM (translate fs) as1
  return $ ifIR var (foldr1 (<:>) x) (foldr1 (<:>) y) <:> valueDefIR var ast
translate fs (AnnAction _ (TillE as)) = do
  let writeVars = Map.keys . Map.filter ((== W).snd) $ safeHeadFact as
  params <- mapM currentName writeVars -- 1. determine the names to call the loop with
  l <- freshName "loop"
  patternVars <- mapM freshName writeVars -- 2. get the next free names for the loop variables
  body <- translateLoop fs l writeVars as -- 3. translate the loop body
  returnVars <- mapM freshName writeVars -- 4 get names to bind result to
  return $ defLoopIR l patternVars body <:> enterLoopIR l params returnVars
translate _ (AnnAction _ E) = error "E at toplevel"
translate _ EmptyAction = return nopIR

translateLoop :: (SynTerm t (AST Varname)) => Facts Usage -> IndexedVar -> [Varname] -> [AnnAction t (Facts (Usage, Modification))] -> FreshVarM IRProgram
translateLoop fs l wVars = (foldr1 (<:>) <$>) . mapM go where
  go :: (SynTerm t (AST Varname)) => AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
  go EmptyAction = do
    params <- mapM currentName wVars
    return $ recCallIR l params
  go (AnnAction _ E) = do
    returnNames <- mapM currentName wVars
    return $ yieldIR returnNames
  go (AnnAction _ (Branch c as1 as2)) = do
    ast <- adjustVars fs $ viewTerm c
    var <- freshName "cond"
    x <- translateLoop fs l wVars as2
    y <- translateLoop fs l wVars as1
    return $ ifIR var x y <:> valueDefIR var ast
  go a = translate fs a

adjustVars :: Facts Usage -> AST Varname -> FreshVarM (AST IndexedVar)
adjustVars fs t = do
  st <- get
  let
    currentName' x = fst $ runFreshVarM (currentName x) st
    f :: Varname -> Usage -> AST IndexedVar
    f x All = Var (currentName' x)
    f x Current = case Map.lookup x fs of
          Just All -> App (Leaf "last") (Var (currentName' x))
          Just Current -> Var $ Plain x
          Nothing -> error "invalid spec"
  return $ replaceVar f t
