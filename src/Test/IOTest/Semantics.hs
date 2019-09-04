{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.IOTest.Semantics (
  Semantics(..),
  evalSemantics,
  execSemantics,
  mapSemantics,
  withSemantics,
  interpret,
  loopEnd,
  ) where

import Prelude hiding (foldMap)

import Test.IOTest.IOrep
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Specification
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Term

import Control.Monad.Extra (ifM)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe

import Data.Bifunctor
import Data.Coerce ( coerce )
import Data.MonoTraversable.Unprefixed

import Test.QuickCheck.GenT

newtype Semantics m a = Semantics { runSemantics :: Context -> m (Maybe a, Context) }
  deriving (Functor, Applicative, Monad, MonadTeletype, MonadState Context, MonadGen) via MaybeT (StateT Context m)

evalSemantics :: Monad m => Semantics m a -> Context -> m (Maybe a)
evalSemantics m c = fst <$> runSemantics m c

execSemantics :: Monad m => Semantics m a -> Context -> m Context
execSemantics m c = snd <$> runSemantics m c

mapSemantics :: (m (Maybe a, Context) -> n (Maybe b, Context)) -> Semantics m a -> Semantics n b
mapSemantics f (Semantics g) = Semantics (f . g)

withSemantics :: (Context -> Context) -> Semantics m a -> Semantics m a
withSemantics f (Semantics g) = Semantics (g . f)

instance MonadWriter NTrace m  => MonadWriter NTrace (Semantics m) where
  writer = coerce . writer @NTrace @(MaybeT (StateT Context m))
  tell = coerce . tell @NTrace @(MaybeT (StateT Context m))
  listen = listen . coerce
  pass = pass . coerce

instance Monad m => Semigroup (Semantics m ()) where
  (<>) = (>>)

instance Monad m => Monoid (Semantics m ()) where
  mempty = pure ()

interpret ::
     (Monad m)
  => (Action -> Semantics m ()) -- handle read
  -> (Action -> Semantics m ()) -- handle write
  -> Specification -- specification
  -> Semantics m ()
interpret r w = foldMap (interpret' r w)

interpret' ::
     (Monad m)
  => (Action -> Semantics m ()) -- handle read
  -> (Action -> Semantics m ()) -- handle write
  -> Action -- action
  -> Semantics m ()
interpret' r _ act@ReadInput{} = r act
interpret' _ w act@WriteOutput{} = w act
interpret' r w (TillE s) =
  let body = interpret r w s
      go = forever body -- repeat until the loop is terminated by an end marker
  in mapSemantics (fmap (first (\Nothing -> Just ()))) go
interpret' r w (Branch c s1 s2) =
  ifM (gets (evalTerm c))
    (interpret r w s2)
    (interpret r w s1)
interpret' _ _ E = loopEnd

loopEnd :: Monad m => Semantics m ()
loopEnd = Semantics (\c -> return (Nothing, c))

-- orphan instances

instance MonadGen m => MonadGen (StateT s m) where
  liftGen g = lift $ liftGen g
  variant n = mapStateT (variant n)
  sized f = let g s = sized (\n -> runStateT (f n) s) in StateT g
  resize n = mapStateT (resize n)
  choose p = lift $ choose p

instance (MonadGen m, Monoid w) => MonadGen (WriterT w m) where
  liftGen g = lift $ liftGen g
  variant n = mapWriterT (variant n)
  sized f = let g = sized (runWriterT . f) in WriterT g
  resize n = mapWriterT (resize n)
  choose p = lift $ choose p

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen g = lift $ liftGen g
  variant n = mapMaybeT (variant n)
  sized f = let g = sized (runMaybeT . f) in MaybeT g
  resize n = mapMaybeT (resize n)
  choose p = lift $ choose p
