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
programIR s = fst $ runFreshVarM (programIR' s) emptyVarInfo

programIR' :: (VarListTerm t Varname, SynTerm t (AST Varname)) => Specification t -> FreshVarM IRProgram
programIR' s = foldr1 (<:>) <$> mapM (translate "main" (rootUsageFacts x)) x
  where x = analyse s

translate :: (SynTerm t (AST Varname)) => Varname -> Facts Usage -> AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
translate scp fs (AnnAction _ (ReadInput x _)) =
  case Map.lookup x fs of
    Just Current ->
      return $ readIR (Plain x)
    Just All -> do
      v <- freshName () "v"
      xk <- currentName () x
      xi <- freshName () x
      return $ case xk of
        Initial _ -> readIR v <:> initialValueIR xi xk v scp
        _ -> readIR v <:> updateIR xi xk v scp
    Nothing -> error "invalid spec"
translate _ _ (AnnAction _ (WriteOutput True _ _)) = return nopIR
translate scp fs (AnnAction _ (WriteOutput False _ (t:_))) = do
  ast <- adjustVars fs $ viewTerm t
  x <- freshName () "t" -- TODO: find way to make this generic
  return $ printIR x <:> valueDefIR x ast scp
translate _ _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate scp fs (AnnAction _ (Branch c as1 as2)) = do
  ast <- adjustVars fs $ viewTerm c
  var <- freshName () "cond"
  x <- scoped $ mapM (translate scp fs) as2
  y <- scoped $ mapM (translate scp fs) as1
  return $ ifIR var (foldr1 (<:>) x) (foldr1 (<:>) y) <:> valueDefIR var ast scp
translate _ fs (AnnAction _ (TillE as)) = do
  let writeVars = Map.keys . Map.filter ((== W).snd) $ safeHeadFact as
  params <- mapM (currentName ()) writeVars -- 1. determine the names to call the loop with
  l <- freshName () "loop"
  enterScope
  patternVars <- mapM (freshName ()) writeVars -- 2. get the next free names for the loop variables
  body <- translateLoop (name l) fs l writeVars as -- 3. translate the loop body
  leaveScope
  returnVars <- mapM (freshName ()) writeVars -- 4 get names to bind result to
  return $ defLoopIR l patternVars body <:> enterLoopIR l params returnVars
translate _ _ (AnnAction _ E) = error "E at toplevel"
translate _ _ EmptyAction = return nopIR

translateLoop :: (SynTerm t (AST Varname)) => Varname -> Facts Usage -> IndexedVar -> [Varname] -> [AnnAction t (Facts (Usage, Modification))] -> FreshVarM IRProgram
translateLoop scp fs l wVars = (foldr1 (<:>) <$>) . mapM go where
  go :: (SynTerm t (AST Varname)) => AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
  go EmptyAction = do
    params <- mapM (currentName ()) wVars
    return $ recCallIR l params
  go (AnnAction _ E) = do
    returnNames <- mapM (currentName ()) wVars
    return $ yieldIR returnNames
  go (AnnAction _ (Branch c as1 as2)) = do
    ast <- adjustVars fs $ viewTerm c
    var <- freshName () "cond"
    x <- scoped $ translateLoop scp fs l wVars as2
    y <- scoped $ translateLoop scp fs l wVars as1
    return $ ifIR var x y <:> valueDefIR var ast scp
  go a = translate scp fs a

adjustVars :: Facts Usage -> AST Varname -> FreshVarM (AST IndexedVar)
adjustVars fs t = do
  st <- get
  let
    currentName' x = fst $ runFreshVarM (currentName () x) st
    f :: Varname -> Usage -> AST IndexedVar
    f x All = Var (currentName' x)
    f x Current = case Map.lookup x fs of
          Just All -> App (Leaf "last") (Var (currentName' x))
          Just Current -> Var $ Plain x
          Nothing -> error "invalid spec"
  return $ replaceVar f t
