{-# LANGUAGE TypeApplications #-}
module HiddenOverflowSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

hiddenOverflowS :: Specification
hiddenOverflowS =  writeOutput [value $ liftOpaque2 ((^),"(^)") (intLit 2) (intLit 64) .>. intLit 0 ]

hiddenOverflowP :: MonadTeletype io => io ()
hiddenOverflowP = print $ 2^64 > 0

spec :: Spec
spec =
  context "overflows in subterms of OutputTerm" $ do
    describe "taskCheck hiddenOverflowP hiddenOverflowS" $ do
      itIsNotSupported "should output overflow warnings" $
        (taskCheckWithOutcome stdArgs{avoidOverflows=False} hiddenOverflowP hiddenOverflowS
          <&> (\o -> isSuccess o && overflowWarnings o > 0) ) `shouldReturn` True

itIsNotSupported s _ = it s $ pendingWith "not yet supported"
