{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Translation (
  buildComputation,
  buildWrongComputation,
  CompType(..),
) where

import Test.IOTest.Environment
import Test.IOTest.Pattern
import Test.IOTest.ValueSet
import Test.IOTest.Utils
import Test.IOTest.Term

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.IOrep
import Test.IOTest.Semantics
import Test.IOTest.Specification

import Data.Maybe
import Data.Proxy
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State
import Control.Monad.Extra ( ifM )

import Type.Reflection
import GHC.TypeLits

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = do
  loopStatus <- evalSemantics (buildComputation' s) (freshEnvironment s)
  case loopStatus of
    Right () -> return ()
    Left Exit -> error "buildComputation: 'throwError Exit' at toplevel"

-- translates to a 'minimal' program satisfying the specification
buildComputation' :: MonadTeletype m => Specification -> Semantics m ()
buildComputation' = interpret build

build :: MonadTeletype m => Action -> Semantics m ()
build (ReadInput x vs) =
  withProxy vs $ \(_ :: Proxy ty) -> do
      v <- unpack @ty <$> getLine
      unless (containsValue vs (Value typeRep v)) (error "encountered out of range input")
      modify (fromJust . update x v)
build (WriteOutput _ [] _) = error "empty list of output options"
build (WriteOutput True _ _) =
  mempty
build (WriteOutput False (p:_) ts) = do
  v <- gets (eval (p,ts))
  putStrLn . render . pPrint $ v
  where
    eval = fillHoles
build _ = return ()

buildWrongComputation :: MonadTeletype m => Specification -> (m (), CompType)
buildWrongComputation s =
  let (ty, compS) = buildWrongComputation' s
      comp = do
        loopStatus <- evalSemantics compS (freshEnvironment s)
        case loopStatus of
          Right () -> return ()
          Left Exit -> error "buildWrongComputation: 'throwError Exit' at toplevel"
  in (comp, ty)

buildWrongComputation' :: MonadTeletype m => Specification -> (CompType, Semantics m ())
buildWrongComputation' = interpret' build'

build' :: MonadTeletype m => ActionSemantic CompType m
build' = ASem
  { r = \x vs ->
      (mempty
      , withProxy vs $ \(_ :: Proxy ty) -> do
          v <- unpack @ty <$> getLine
          unless (containsValue vs (Value typeRep v)) (error "encountered out of range input")
          modify (fromJust . update x v)
      )
  , w = \opt ps ts -> if null ps then error "empty list of output options" else
      (mempty
      , do
          v <- gets (fillHoles (head ps,ts))
          putStrLn . render . pPrint $ v
      )
  , b = \_ (_,(_,p1)) (_,(_,p2)) -> ((Wrong,p2),(Wrong,p1)) -- swap branches
  , l = snd
  , e = id
  }

data CompType = Wrong | NotWrong deriving (Eq, Show)

instance Semigroup CompType where
  Wrong <> _ = Wrong
  _ <> Wrong = Wrong
  _ <> _ = NotWrong

instance Monoid CompType where
  mempty = NotWrong
