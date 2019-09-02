{-# LANGUAGE TupleSections #-}
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
  continue,
  loopEnd,
  ) where

import Test.IOTest.IOrep
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Specification
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Term

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Applicative

import Data.Bifunctor
import Data.Coerce ( coerce )

import Test.QuickCheck.GenT

newtype Semantics m a = Semantics { runSemantics :: Context -> m (Maybe a, Context) }
  deriving (Functor, Applicative, Monad, MonadTeletype, MonadState Context) via MaybeT (StateT Context m)

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

instance MonadGen m => MonadGen (Semantics m) where
  liftGen g = Semantics (\s -> first Just . (, s) <$> liftGen g)
  variant n = mapSemantics (variant n)
  sized f = let g s = sized (\n -> runSemantics (f n) s) in Semantics g
  resize n = mapSemantics (resize n)
  choose p = Semantics (\s -> first Just . (, s) <$> choose p)

instance (Monad m, Semigroup a) => Semigroup (Semantics m a) where
  (<>) = liftA2 (<>)

instance (Monad m, Monoid a) => Monoid (Semantics m a) where
  mempty = pure mempty

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
  in mapSemantics  (fmap . first $ void . Just) go
interpret' r w (Branch p s1 s2) = do
  cond <- gets (evalTerm p)
  if cond
    then interpret r w s2
    else interpret r w s1
interpret' _ _ E = loopEnd

loopEnd :: Monad m => Semantics m ()
loopEnd = Semantics (\c -> return (Nothing, c))

continue :: Monad m => Semantics m ()
continue = mempty

-- orphan instances

instance MonadGen m => MonadGen (StateT s m) where
  liftGen g = StateT (\s -> (, s) <$> liftGen g)
  variant n = mapStateT (variant n)
  sized f = let g s = sized (\n -> runStateT (f n) s) in StateT g
  resize n = mapStateT (resize n)
  choose p = StateT (\s -> (, s) <$> choose p)

instance (MonadGen m, Monoid w) => MonadGen (WriterT w m) where
  liftGen g = WriterT ((, mempty) <$> liftGen g)
  variant n = mapWriterT (variant n)
  sized f = let g = sized (runWriterT . f) in WriterT g
  resize n = mapWriterT (resize n)
  choose p = WriterT ((, mempty) <$> choose p)

instance MonadGen m => MonadGen (MaybeT m) where
  liftGen g = MaybeT (Just <$> liftGen g)
  variant n = mapMaybeT (variant n)
  sized f = let g = sized (runMaybeT . f) in MaybeT g
  resize n = mapMaybeT (resize n)
  choose p = MaybeT (Just <$> choose p)
