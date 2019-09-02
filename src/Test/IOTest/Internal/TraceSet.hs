{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Internal.TraceSet (
  traceGen
) where

import Prelude hiding (putStrLn, getLine)

import Test.IOTest.IOrep
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.Context
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Internal.Specification

-- import Test.QuickCheck hiding (Positive,Function)
import Test.QuickCheck.GenT
import qualified Data.Set as S
import           Data.Maybe

import           Data.Coerce                    ( coerce )

import System.Random

import Control.Monad.State
import Control.Monad.Cont
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import           Control.Applicative

traceGen :: MonadGen m => Specification -> m NTrace
traceGen s = execWriterT $ evalStateT (runMaybeT $ runAppMonoid $ traceGen' s) (freshContext s)

traceGen' ::
     MonadGen m => Specification
  -> AppMonoid (MaybeT (StateT Context (WriterT NTrace m))) ()
traceGen' = foldMap traceGen''

traceGen'' ::
     MonadGen m => Action
  -> AppMonoid (MaybeT (StateT Context (WriterT NTrace m))) ()
traceGen'' act = sized $ \size ->
  case act of
    ReadInput x vs -> do
      seed <- choose (minBound, maxBound)
      let v = valueOf vs (mkStdGen seed) -- TODO: is there a better way of doing this?
      modify (fromMaybe (error "type mismatch on context update") . update x v)
      tell $ Trace [ProgRead (show v)]
      -- FIXME: clean up according to paper definition?
    WriteOutput pxy opt ps ts -> do
      d <- get
      let v1 = S.fromList ((\p -> fillHoles pxy p ts d) <$> ps)
          v1' = if opt then S.insert emptyPattern v1 else v1
      unless (null v1') $ tell (Trace [ProgWrite v1'])
    TillE s ->
      let body = traceGen' s
          go = forever body
      in AppMonoid $ mapMaybeT (fmap (void . Just)) (runAppMonoid go)
    Branch p s11 s12 -> do
      d <- get
      if evalTerm p d
        then traceGen' s12
        else traceGen' s11
    E -> AppMonoid $ MaybeT $ return Nothing

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

instance MonadGen m => MonadGen (ContT r m) where
  liftGen g = ContT $ (>>=) $ liftGen g
  variant n = mapContT (variant n)
  sized f = let mr k = sized @m (\n -> runContT (f n) k) in ContT mr
  resize n = mapContT (resize n)
  choose p = ContT $ (>>=) $ choose p

newtype AppMonoid f a = AppMonoid { runAppMonoid :: f a }
  deriving (Functor, Applicative, Monad, MonadGen, MonadTeletype) via f

instance (Applicative f, Semigroup a) => Semigroup (AppMonoid f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (AppMonoid f a) where
  mempty = pure mempty

instance MonadState Context m  => MonadState Context (AppMonoid m) where
  state = coerce . state @Context @m

instance MonadWriter NTrace m  => MonadWriter NTrace (AppMonoid m) where
  writer = coerce . writer @NTrace @m
  tell = coerce . tell @NTrace @m
  listen = listen . coerce
  pass = pass . coerce
