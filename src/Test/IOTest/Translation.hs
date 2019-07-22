{-# LANGUAGE TypeApplications #-}
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
import Test.IOTest.IOtt

import Control.Monad (void)
import           Data.Maybe
import           System.Random

buildProgram :: Specification -> IOtt ()
buildProgram s = void $ translate s (freshContext s)

-- translates to a 'minimal' program satisfying the specification
translate :: Specification -> Context -> IOtt (Context,LoopEnd)
translate (ReadInput x vs) d =
  elimValueSet vs (error "proxy RandomGen sampled" :: StdGen)
    (\ p _ (_ :: ty) -> do
      v <- unpack @_ @ty p <$> getLine
      return (fromJust $ update d x (Value p v), No)
  )
translate (WriteOutput _ _ [] _) _ = error "empty list of output options"
translate (WriteOutput _ True _ _) d = return (d,No)
translate (WriteOutput pxy False (p:_) ts) d = do
  print $ fillHoles pxy p ts d
  return (d, No)
translate E d = return (d, Yes)
translate Nop d = return (d, No)
translate (TillE s) d =
  let body = translate s
      go d' = do
        (d'', end) <- body d'
        case end of
          Yes -> return (d'', No)
          No -> go d''
  in go d
translate (Branch p s1 s2) d =
  if evalTerm p d
    then translate s2 d
    else translate s1 d
translate (s1 :<> s2) d = translate s1 d >>= (\(d',_) -> translate s2 d')

data LoopEnd = Yes | No
