{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Test.IOTest.Semantics (
  Semantics(..),
  evalSemantics,
  interpret,
  interpret',
  ) where

import Prelude hiding (foldMap)

import Test.IOTest.IOrep
import Test.IOTest.Environment
import Test.IOTest.Specification
import Test.IOTest.Term

import Control.Monad.Extra (ifM)
import Control.Monad.State
import Control.Monad.Except

import Data.MonoTraversable.Unprefixed

import Test.QuickCheck.GenT

newtype Semantics m a = Semantics { runSemantics :: Environment -> m (Either Exit a, Environment) }
  deriving (Functor, Applicative, Monad, MonadTeletype, MonadState Environment, MonadError Exit, MonadGen)
    via ExceptT Exit (StateT Environment m)

evalSemantics :: Monad m => Semantics m a -> Environment -> m (Either Exit a)
evalSemantics m c = fst <$> runSemantics m c

instance Monad m => Semigroup (Semantics m ()) where
  (<>) = (>>)

instance Monad m => Monoid (Semantics m ()) where
  mempty = return ()

interpret ::
     (Monad m, SemTerm t)
  => (Action t -> Semantics m ()) -- behavior for read and write actions
  -> Specification t -- specification
  -> Semantics m ()
interpret sem =
  foldMap (\act -> case act of
                     ReadInput{}   -> sem act
                     WriteOutput{} -> sem act
                     _             -> structure (interpret sem) act
          )

interpret' ::
     (Monad m, SemTerm t)
  => ((Action t, Semantics m ()) -> Semantics m ()) -- extra behavior for single action that can use or completly discard the structural interpretation
  -> Specification t -- specification
  -> Semantics m ()
interpret' sem =
  foldMap (modifyInterpretation (structure (interpret' sem)) sem)

modifyInterpretation :: (Action t -> a) -> ((Action t, a) -> b) -> Action t -> b
modifyInterpretation i f act = f (act, i act)

structure :: (Monad m, SemTerm t) => (Specification t -> Semantics m ()) -> Action t -> Semantics m ()
structure ff (TillE s) =
  let body = ff s
      loop = forever body -- repeat until the loop is terminated by an end marker
  in catchError loop (\Exit -> return ())
structure ff (Branch c s1 s2) =
  ifM (gets (evalTerm c))
    (ff s2)
    (ff s1)
structure _ E = throwError Exit
structure _ _ = error "not a structure action"

-- orphan instances

instance MonadGen m => MonadGen (StateT s m) where
  liftGen g = lift $ liftGen g
  variant n = mapStateT (variant n)
  sized f = let g s = sized (\n -> runStateT (f n) s) in StateT g
  resize n = mapStateT (resize n)
  choose p = lift $ choose p

instance MonadGen m => MonadGen (ExceptT Exit m) where
  liftGen g = lift $ liftGen g
  variant n = mapExceptT (variant n)
  sized f = let g = sized (runExceptT . f) in ExceptT g
  resize n = mapExceptT (resize n)
  choose p = lift $ choose p
