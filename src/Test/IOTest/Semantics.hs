{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Test.IOTest.Semantics (
  Semantics(..),
  ActionSemantic(..),
  evalSemantics,
  interpret,
  interpret',
  ) where

import Prelude hiding (foldMap)

import Test.IOTest.IOrep
import Test.IOTest.Environment
import Test.IOTest.Specification
import Test.IOTest.Term
import Test.IOTest.ValueSet
import Test.IOTest.Pattern
import Test.IOTest.Utils

import Control.Monad.Extra (ifM)
import Control.Monad.State
import Control.Monad.Except

import Data.MonoTraversable.Unprefixed

import Test.QuickCheck.GenT

newtype Semantics m a = Semantics { runSemantics :: Environment -> m (Either Exit a, Environment) }
  deriving (Functor, Applicative, Monad, MonadTeletype, MonadState Environment, MonadError Exit, MonadGen) via ExceptT Exit (StateT Environment m)

evalSemantics :: Monad m => Semantics m a -> Environment -> m (Either Exit a)
evalSemantics m c = fst <$> runSemantics m c

instance Monad m => Semigroup (Semantics m ()) where
  (<>) = (>>)

instance Monad m => Monoid (Semantics m ()) where
  mempty = return ()

interpret ::
     (Monad m)
  => (Action -> Semantics m ()) -- extra behavior for single action
  -> Specification -- specification
  -> Semantics m ()
interpret sem = snd . interpret' (funcToASem @() sem) -- (\act ((),struct) -> ((),sem act >> struct))

interpret' ::
     (Monad m, Monoid flag)
  => ActionSemantic flag m -- extra behavior for single action that can use or completly discard the structural interpretation
  -> Specification -- specification
  -> (flag, Semantics m ())
interpret' sem = foldMap (interpretAction' sem)

interpretAction' ::
     (Monad m, Monoid flag)
  => ActionSemantic flag m -- extra behavior for single action that can use or completly discard the structural interpretation
  -> Action -- action
  -> (flag, Semantics m ())
interpretAction' sem = structure
  where
    --structure :: Action -> (flag, Semantics m ())
    structure (ReadInput x ty) = r sem x ty
    structure (WriteOutput opt ps ts) = w sem opt ps ts
    structure (TillE s) =
      let (flag, body) = interpret' sem s
          loop = forever body -- repeat until the loop is terminated by an end marker
      in l sem (s , (flag, catchError loop (\Exit -> return ())))
    structure (Branch c s1 s2) =
      let (flag2, s2') = interpret' sem s2
          (flag1, s1') = interpret' sem s1
          ((fx,sx),(fy,sy)) = b sem c (s1, (flag1, s1')) (s2, (flag2, s2'))
      in (fx <> fy, ifM (gets (evalTerm c)) sy sx)
    structure E = e sem (mempty, throwError Exit)

data ActionSemantic flag m = ASem
  { r :: Varname -> ValueSet -> (flag, Semantics m ())
  , w :: forall a. StringEmbedding a => Bool -> [TermPattern] -> [Term a] -> (flag, Semantics m ())
  , b :: Term Bool -> (Specification,(flag, Semantics m ())) -> (Specification, (flag, Semantics m ())) -> ((flag, Semantics m ()), (flag, Semantics m ()))
  , l :: (Specification, (flag, Semantics m ())) -> (flag, Semantics m ())
  , e :: (flag, Semantics m ()) -> (flag, Semantics m ())
  }

idSem :: (Monoid flag, Monad m) => ActionSemantic flag m
idSem = ASem
  { r = const . const mempty
  , w = const . const . const  mempty
  , b = \_ (_,x) (_,y) -> (x,y)
  , l = snd
  , e = id
  }

funcToASem :: Monoid flag => (Action -> Semantics m ()) -> ActionSemantic flag m
funcToASem f = ASem
  { r = \x ty -> (mempty, f $ ReadInput x ty)
  , w = \opt ps ts -> (mempty, f $ WriteOutput opt ps ts)
  , b = \_ (_,x) (_,y) -> (x,y)
  , l = snd
  , e = id
  }

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
