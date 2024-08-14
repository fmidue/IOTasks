{-# LANGUAGE TypeApplications #-}
module PingPongSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.IOTasks

import Data.Functor ((<&>))

pingPongSpec :: Specification
pingPongSpec =
  readInput x str AssumeValid <>
  branch (currentValue x .==. listLit "Ping")
    (writeOutput [text "Pong"])
    (branch (currentValue x .==. listLit "Pong")
      (writeOutput [text "Ping"])
      nop
    )
  where
    x = stringVar "x"

pingPongProg :: MonadTeletype m => m ()
pingPongProg = do
  str <- getLine
  case str of
    "Ping" -> putStrLn "Pong"
    "Pong" -> putStrLn "Ping"
    _ -> pure ()

spec :: Spec
spec =
  context "string inputs" $ do
    describe "pingPongProg" $ do
      it "satisfies pingPongProg" $
        (taskCheckOutcome pingPongProg pingPongSpec <&> isSuccess) `shouldReturn` True
