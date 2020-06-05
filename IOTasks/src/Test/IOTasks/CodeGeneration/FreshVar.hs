{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.CodeGeneration.FreshVar where

import Test.IOTasks.CodeGeneration.Analysis (Varname)

import Control.Monad.State

import Data.Maybe (fromMaybe)
import Data.Functor.Identity

data IndexedVar
  = Initial Varname
  | Plain Varname
  | Indexed Varname Int
  deriving (Eq,Ord)

instance Show IndexedVar where
  show = name

-- !!! not necessarily unique in the sense that one can define ("x1",1) and ("x",11)
name :: IndexedVar -> String
name (Initial _) = "[]"
name (Plain x) = x
name (Indexed x i) = x ++ show i

baseName :: IndexedVar -> Varname
baseName (Initial x) = x
baseName (Plain x) = x
baseName (Indexed x _) = x

changeBaseName :: IndexedVar -> Varname -> IndexedVar
changeBaseName (Initial _) x = Initial x
changeBaseName (Plain _) x = Plain x
changeBaseName (Indexed _ n) x = Indexed x n

inc :: IndexedVar -> Int -> IndexedVar
inc (Initial x) n = Indexed x n
inc (Indexed x i) n = Indexed x (i+n)
inc (Plain _) _ = error "can not increase index of plain variable"

-- stores next fresh index and a stack of most recent indecies for the current/surounding scopes
-- invariant: stack is never empty
newtype FreshVarM a = FreshVarM { runFreshVarM :: VarInfo -> (a,VarInfo) }
  deriving (Functor, Applicative, Monad, MonadState VarInfo) via (StateT VarInfo Identity)

data VarInfo = VarInfo { fresh :: [(Varname,Int)], current :: [(Varname,Int)], parentScope :: Maybe VarInfo }

emptyVarInfo :: VarInfo
emptyVarInfo = VarInfo [] [] Nothing

updateContext :: Eq k => (k, Int) -> [(k,Int)] -> [(k,Int)]
updateContext (k,v) [] = [(k,v)]
updateContext (k,v) ((k',v') : xs)
  | k == k' = (k',v) : xs
  | otherwise = (k',v') : updateContext (k,v) xs

-- generating a fresh name under the assumption that user defined variables dont end in numberic sequences
-- TODO: not a very good assumption
freshName :: () -> Varname -> FreshVarM IndexedVar
freshName _ v = do
  i <- gets $ fromMaybe 1 . lookup v . fresh
  modify $ \(VarInfo fr cu p) -> VarInfo (updateContext (v,i+1) fr) (updateContext (v,i) cu) p
  return $ Indexed v i

currentName :: () -> Varname -> FreshVarM IndexedVar
currentName _ v = do
  i <- gets $ fromMaybe 0 . lookup v . current
  case i of
    0 -> return $ Initial v
    _ -> return $ Indexed v i

enterScope :: FreshVarM ()
enterScope = modify $ \info -> info { parentScope  = Just info }

leaveScope :: FreshVarM ()
leaveScope = modify $ \case
  VarInfo fr _ (Just p) -> p { fresh = fr }
  VarInfo{} -> error "leaveScope: no parent scope"

scoped :: FreshVarM a -> FreshVarM a
scoped m = do
  enterScope
  a <- m
  leaveScope
  return a
