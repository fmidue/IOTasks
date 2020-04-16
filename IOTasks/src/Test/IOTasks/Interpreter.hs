{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Interpreter (
  buildComputation,
  -- buildWrongComputation,
) where

import Data.Environment
import Test.IOTasks.Pattern
import Test.IOTasks.ValueSet
import Test.IOTasks.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTasks.IOrep
import Test.IOTasks.Semantics
import Test.IOTasks.Specification
import Data.Term (SemTerm(..),VarListTerm)

import Data.Maybe
import Data.Proxy
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State
import Control.Monad.Extra ( ifM )

import Type.Reflection

buildComputation :: (SemTerm t (Environment Varname), VarListTerm t Varname) => MonadTeletype m => Specification t -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment (specVars s))
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: 'throwError Exit' at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: (MonadTeletype m, SemTerm t (Environment Varname)) => Specification t -> Semantics m ()
buildComputation' = interpret (build InputRangeCheck)

data Checks = NoCheck | InputRangeCheck deriving (Eq, Show)

build :: (MonadTeletype m, SemTerm t (Environment Varname)) => Checks -> Action (Specification t) t -> Semantics m ()
build c (ReadInput x vs) =
  withProxy vs $ \(_ :: Proxy ty) -> do
      v <- unpack @ty <$> getLine
      when (c == InputRangeCheck && not (containsValue vs (Value typeRep pack v)))
        (error "encountered out of range input")
      modify (fromJust . store x pack v)
build _ (WriteOutput _ [] _) = error "empty list of output options"
build _ (WriteOutput True _ _) =
  mempty
build _ (WriteOutput False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . fixLinebreaks . render . pPrint $ v
  where
    eval = fillHoles
    fixLinebreaks :: String -> String
    fixLinebreaks "" = ""
    fixLinebreaks ('\\':'\\':cs) = '\\' : fixLinebreaks cs
    fixLinebreaks ('\\':'n':cs) = '\n' : fixLinebreaks cs
    fixLinebreaks (c:cs) = c : fixLinebreaks cs
build _ _ = error "not a read or write action"

buildWrongComputation :: (MonadTeletype m, SemTerm t (Environment Varname), VarListTerm t Varname) => Specification t -> m ()
buildWrongComputation s = do
  loopStatus <- evalSemantics (buildWrongComputation' s) (freshEnvironment (specVars s))
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildWrongComputation: 'throwError Exit' at toplevel"

buildWrongComputation' :: (MonadTeletype m, SemTerm t (Environment Varname), VarListTerm t Varname) => Specification t -> Semantics m ()
buildWrongComputation' = interpret' buildWrong

buildWrong :: (MonadTeletype m, SemTerm t (Environment Varname), VarListTerm t Varname) => (Action (Specification t) t, Semantics m ()) -> Semantics m ()
buildWrong (r@ReadInput{}, _) = build NoCheck r
buildWrong (w@WriteOutput{}, _) = build NoCheck w
buildWrong (Branch c s1 s2, _) =
  ifM (gets (evalTerm c))
    -- swap branches
    (buildWrongComputation s2)
    (buildWrongComputation s1)
buildWrong (TillE _, p) = p
buildWrong (E,p) = p
