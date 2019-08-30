{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTest.Internal.TraceSet (
  traceGen
) where

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
import           System.Random

import Data.Functor.Identity

import System.Random

import Control.Monad.State
import Control.Monad.Cont

traceGen :: MonadGen m => Specification -> m NTrace
traceGen = \s -> evalStateT (traceGen' s kI) (freshContext s)
  where kI :: MonadGen m => KAction -> StateT Context m NTrace
        kI Continue = return $ ProgWrite (S.singleton emptyPattern) Stop
        kI Exit = error "ill formed spec!"

-- ContT ((a -> m r) -> m r)

traceGen' ::
     MonadGen m =>
  -- specification
     Specification
  -- contiuation
  -> (KAction -> StateT Context m NTrace)
  -- variable context
  -> StateT Context m NTrace
traceGen' spec k = sized $ \size ->
  case spec of
    (ReadInput x vs : s') -> do
      seed <- choose (minBound, maxBound)
      let v = valueOf vs (mkStdGen seed) -- TODO: is there a better way of doing this?
      modify (fromMaybe (error "type mismatch on context update") . update x v)
      t' <- traceGen' s' k
      return $ ProgWrite (S.singleton emptyPattern) $ ProgRead (show v) t'
      -- FIXME: clean up according to paper definition?
    (WriteOutput pxy opt ps ts : s') -> do
      d <- get
      let v1 = S.fromList ((\p -> fillHoles pxy p ts d) <$> ps)
          v1' = if opt then S.insert emptyPattern v1 else v1
      t <- traceGen' s' k
      let (ProgWrite v2 t') = t
      let v = S.map (uncurry (<>)) $ S.cartesianProduct v1' v2
      return $ ProgWrite v t'
    (TillE s : s') ->
      let k' Continue = traceGen' s k'
          k' Exit = traceGen' s' k
      in traceGen' s k'
    (Branch p s11 s12 : s2) -> do
      d <- get
      if evalTerm p d
        then traceGen' (s12 <> s2) k
        else traceGen' (s11 <> s2) k
    (E : _) -> k Exit
    [] -> k Continue

data KAction = Continue | Exit deriving (Eq,Show)

instance MonadGen m => MonadGen (StateT s m) where
  liftGen g = StateT (\s -> (, s) <$> liftGen g)
  variant n = mapStateT (variant n)
  sized f = let g s = sized (\n -> runStateT (f n) s) in StateT g
  resize n = mapStateT (resize n)
  choose p = StateT (\s -> (, s) <$> choose p)

instance MonadGen m => MonadGen (ContT r m) where
  liftGen g = ContT $ (>>=) $ liftGen g
  variant n = mapContT (variant n)
  sized f = let mr k = sized @m (\n -> runContT (f n) k) in ContT mr
  resize n = mapContT (resize n)
  choose p = ContT $ (>>=) $ choose p
