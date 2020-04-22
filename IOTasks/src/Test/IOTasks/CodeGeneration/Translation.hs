{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTasks.CodeGeneration.Translation where

import Test.IOTasks.Specification
import Test.IOTasks.ValueSet (withProxy)
import Test.IOTasks.CodeGeneration.IRNew
import Test.IOTasks.CodeGeneration.Analysis

import Data.Term
import Data.Term.Typed.AST
import Data.Term.Liftable (unHO)
import qualified Data.Term.Liftable.Prelude as T (last)

import Control.Monad.State

import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as Map

import Type.Reflection (Typeable, typeRep, TypeRep)
import Text.PrettyPrint.HughesPJClass hiding (first, (<>))

printProgram :: (VarListTerm t Varname, SynTermTyped t (AST Varname)) => Specification t -> Doc
printProgram = hang (text "p :: IO ()" $$ text "p = do") 2 . printProgram . programIR

programIR :: (VarListTerm t Varname, SynTermTyped t (AST Varname)) => Specification t -> IRProgram
programIR s = foldr1 (<:>) . fst $ runFreshVarM (mapM (translate (rootUsageFacts x)) x) initState
  where x = analyse s

newtype IndexedVar = IVar (Varname,Int) deriving (Eq,Ord,Show)

-- !!! not necessarily unique in the sense that one can define ("x1",1) and ("x",11)
name :: IndexedVar -> String
name (IVar (_,0)) = "[]" -- this feels wrong somehow
name (IVar (x,i)) = x ++ show i

-- stores next fresh index and a stack of most recent indecies for the current/surounding scopes
-- invariant: stack is never empty
newtype FreshVarM a = FreshVarM { runFreshVarM :: [(Varname, Int)] -> (a,[(Varname, Int)]) }
  deriving (Functor, Applicative, Monad, MonadState [(Varname, Int)]) via (StateT [(Varname, Int)] Identity)

initState :: [(Varname,Int)]
initState = []

updateContext :: Eq k => (k, Int) -> [(k,Int)] -> [(k,Int)]
updateContext (k,v) [] = [(k,v)]
updateContext (k,v) ((k',v') : xs)
  | k == k' = (k',v) : xs
  | otherwise = (k',v') : updateContext (k,v) xs

-- generating a fresh name under the assumption that user defined variables dont end in numberic sequences
-- TODO: not a very good assumption
freshName :: Varname -> FreshVarM IndexedVar
freshName v = do
  i <- gets $ (+1) . fromMaybe 0 . lookup v
  modify $ updateContext (v, i)
  return $ IVar (v, i)

currentName :: Varname -> FreshVarM IndexedVar
currentName v = do
  i <- gets $ fromMaybe 0 . lookup v
  return $ IVar (v, i)

translate :: (VarListTerm t Varname, SynTermTyped t (AST Varname)) => Facts Usage -> AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
translate fs (AnnAction _ (ReadInput x vs)) =
  case Map.lookup x fs of
    Just Current -> do
      n <- freshName x
      return $ readIR (name n)
    Just All -> do
      v <- freshName "v"
      xk <- currentName x
      xi <- freshName x
      return $ case xk of
        IVar (_,0) -> readIR (name v) <:> withProxy vs (initialValueIR (name xi) (name xk) (name v))
        _ -> readIR (name v) <:> withProxy vs (updateIR (name xi) (name xk) (name v))
    Nothing -> error "invalid spec"
translate _ (AnnAction _ (WriteOutput True _ _)) = return nopIR
translate fs (AnnAction _ (WriteOutput False _ (t:_))) = do
  ast <- adjustVars fs $ viewTermTyped t
  x <- freshName "t" -- TODO: find way to make this generic
  return $ printIR (name x) <:> valueDefIR (name x) ast
translate _ (AnnAction _ (WriteOutput False _  [])) = error "invalid spec"
translate fs (AnnAction _ (Branch c as1 as2)) = do
  ast <- adjustVars fs $ viewTermTyped c
  var <- name <$> freshName "cond"
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
  return $ defLoopIR (name l) (map name patternVars) body <:> enterLoopIR (name l) (map name params) (map name returnVars)
translate _ (AnnAction _ E) = error "E at toplevel"
translate _ EmptyAction = return nopIR

translateLoop :: (VarListTerm t Varname, SynTermTyped t (AST Varname)) => Facts Usage -> IndexedVar -> [Varname] -> [AnnAction t (Facts (Usage, Modification))] -> FreshVarM IRProgram
translateLoop fs l wVars = (foldr1 (<:>) <$>) . mapM go where
  go :: (VarListTerm t Varname, SynTermTyped t (AST Varname)) => AnnAction t (Facts (Usage, Modification)) -> FreshVarM IRProgram
  go EmptyAction = do
    params <- mapM currentName wVars
    return $ recCallIR (name l) (map name params)
  go (AnnAction _ E) = do
    returnNames <- mapM currentName wVars
    return $ returnIR (map name returnNames)
  go (AnnAction _ (Branch c as1 as2)) = do
    ast <- adjustVars fs $ viewTermTyped c
    var <- name <$> freshName "cond"
    x <- translateLoop fs l wVars as2
    y <- translateLoop fs l wVars as1
    return $ ifIR var x y <:> valueDefIR var ast
  go a = translate fs a

adjustVars :: Facts Usage -> AST Varname a -> FreshVarM (AST Varname a)
adjustVars fs t = do
  st <- get
  let
    currentName' x = fst $ runFreshVarM (name <$> currentName x) st
    f :: forall a. Typeable a => Varname -> TypeRep a -> AST Varname a
    f x r
      | "_A" `isSuffixOf` x = Var (currentName' $ take (length x - 2) x) r
      | otherwise = case Map.lookup x fs of
          Just All -> App (unHO T.last) (Var (currentName' x) (typeRep @[a]))
          Just Current -> Var (currentName' x) r
          Nothing -> error "invalid spec"
  return $ replaceVar f t
