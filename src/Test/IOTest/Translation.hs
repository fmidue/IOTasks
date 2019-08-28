{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Translation (
  buildProgram
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

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

buildProgram :: TeletypeM m => Specification -> m ()
buildProgram s = void $ evalStateT (translate s) (freshContext s)

-- translates to a 'minimal' program satisfying the specification
translate :: TeletypeM m => Specification -> StateT Context m LoopEnd
translate (ReadInput x vs) =
  elimValueSet vs (error "proxy RandomGen sampled" :: StdGen)
    (\ p _ (_ :: ty) -> do
      v <- unpack @_ @ty p <$> lift getLine
      modify (\d -> fromJust $ update d x (Value p v))
      return No
  )
translate (WriteOutput _ _ [] _) = error "empty list of output options"
translate (WriteOutput _ True _ _) = return No
translate (WriteOutput pxy False (p:_) ts) = do
  d <- get
  lift . putStrLn . render . pPrint $ fillHoles pxy p ts d
  return No
translate E = return Yes
translate Nop = return No
translate (TillE s) =
  let body = translate s
      go = do
        end <- body
        case end of
          Yes -> return No
          No -> go
  in go
translate (Branch p s1 s2) = do
  d <- get
  if evalTerm p d
    then translate s2
    else translate s1
translate (s1 :<> s2) =
  translate s1 >>=
    \case Yes -> return Yes
          No -> translate s2

data LoopEnd = Yes | No
