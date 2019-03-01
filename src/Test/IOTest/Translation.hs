module Test.IOTest.Translation (
  buildProgram
) where

import Prelude hiding (putStrLn,getLine,print)
import Test.IOTest.Language
import Test.IOTest.IOtt
import Test.IOTest.Context

import Control.Monad (void)

buildProgram :: Specification -> IOtt ()
buildProgram s = void $ translate s (freshContext s)

translate :: Specification -> Context -> IOtt (Context,LoopEnd)
translate (ReadInput x _) d = do
  v <- read <$> getLine
  return (update d x v, No)
translate (WriteOutput []) _ = error "empty list of output options"
translate (WriteOutput (Optional:_)) d = return (d,No)
translate (WriteOutput (f:_)) d = do
  print $ evalF d f
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
  if evalP d p
    then translate s2 d
    else translate s1 d
translate (s1 :<> s2) d = translate s1 d >>= (\(d',_) -> translate s2 d')

data LoopEnd = Yes | No
