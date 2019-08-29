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
import System.Random
import Text.PrettyPrint.HughesPJClass

import Control.Monad.State
import Control.Monad.Trans.Maybe

buildComputation :: MonadTeletype m => Specification -> m ()
buildComputation s = void $ evalStateT (runMaybeT $ interpret s) (freshContext s)

-- translates to a 'minimal' program satisfying the specification
interpret :: MonadTeletype m => Specification -> MaybeT (StateT Context m) ()
interpret (ReadInput x vs) =
  elimValueSet vs (error "proxy RandomGen sampled" :: StdGen)
    (\ p _ (_ :: ty) -> do
      v <- unpack @_ @ty p <$> getLine
      modify (fromJust . update x (Value p v))
  )
interpret (WriteOutput _ _ [] _) = error "empty list of output options"
interpret (WriteOutput _ True _ _) = continue
interpret (WriteOutput pxy False (p:_) ts) =
  get >>= (putStrLn . render . pPrint . fillHoles pxy p ts)
interpret E = loopEnd
interpret Nop = continue
interpret (TillE s) =
  let body = interpret s
      go = forever body -- repeat until the loop is terminated by an end marker
  in mapMaybeT (fmap (void . Just)) go
interpret (Branch p s1 s2) = do
  cond <- gets (evalTerm p)
  if cond
    then interpret s2
    else interpret s1
interpret (s1 :<> s2) = interpret s1 >> interpret s2

loopEnd :: Monad m => MaybeT m ()
loopEnd = MaybeT $ return Nothing

continue :: Monad m => m ()
continue = return ()
