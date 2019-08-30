{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Translation (
  buildComputation
) where

import Test.IOTest.Internal.Context
import Test.IOTest.Internal.Pattern
import Test.IOTest.Internal.Term
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Utils

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.Internal.Specification
import Test.IOTest.IOrep

import Control.Monad (void)
import Data.Maybe
import           Data.Coerce                    ( coerce )
import System.Random
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Control.Monad.State
import Control.Monad.Trans.Maybe
import           Control.Applicative

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = void $ evalStateT (runMaybeT $ runAppMonoid $ interpret s) (freshContext s)

-- translates to a 'minimal' program satisfying the specification
interpret ::MonadTeletype m => Specification -> AppMonoid (MaybeT (StateT Context m)) ()
interpret = foldMap interpret'

interpret' :: MonadTeletype m => Action -> AppMonoid (MaybeT (StateT Context m)) ()
interpret' (ReadInput x vs) =
  elimValueSet vs (error "proxy RandomGen sampled" :: StdGen)
    (\ p _ (_ :: ty) -> do
      v <- unpack @_ @ty p <$> getLine
      modify (fromJust . update x (Value p v))
  )
interpret' (WriteOutput _ _ [] _) = error "empty list of output options"
interpret' (WriteOutput _ True _ _) = continue
interpret' (WriteOutput pxy False (p:_) ts) =
  get >>= (putStrLn . render . pPrint . fillHoles pxy p ts)
interpret' E = AppMonoid loopEnd
interpret' (TillE s) =
  let body = interpret s
      go = forever body -- repeat until the loop is terminated by an end marker
  in AppMonoid $ mapMaybeT (fmap (void . Just)) (runAppMonoid go)
interpret' (Branch p s1 s2) = do
  cond <- gets (evalTerm p)
  if cond
    then interpret s2
    else interpret s1

loopEnd :: Monad m => MaybeT m ()
loopEnd = MaybeT $ return Nothing

continue :: Monad m => m ()
continue = return ()

newtype AppMonoid f a = AppMonoid { runAppMonoid :: f a }
  deriving (Functor, Applicative, Monad) via f

instance (Applicative f, Semigroup a) => Semigroup (AppMonoid f a) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid a) => Monoid (AppMonoid f a) where
  mempty = pure mempty

instance MonadState Context m  => MonadState Context (AppMonoid m) where
  state = coerce . state @Context @m

instance MonadTeletype m  => MonadTeletype (AppMonoid m) where
  putStrLn = coerce . putStrLn @m
  getLine = coerce $ getLine @m
