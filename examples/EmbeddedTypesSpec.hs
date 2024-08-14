{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module EmbeddedTypesSpec where

import Prelude hiding (readLn, print, getChar)

import Test.Hspec
import Test.IOTasks
import qualified Test.IOTasks.Random as Random

import Data.Functor ((<&>))

data Column = A | B | C
  deriving (Eq, Read, Show)

data Row = X | Y | Z
  deriving (Eq, Read, Show)

type Pos = (Column,Row)

instance Embeddable Pos where
  asOriginal :: Integer -> Pos
  asOriginal 1 = (A,X)
  asOriginal 2 = (A,Y)
  asOriginal 3 = (A,Z)
  asOriginal 4 = (B,X)
  asOriginal 5 = (B,Y)
  asOriginal 6 = (B,Z)
  asOriginal 7 = (C,X)
  asOriginal 8 = (C,Y)
  asOriginal 9 = (C,Z)
  asOriginal n = error $ "invalid Pos: " ++ show n

  asInteger :: Pos -> Integer
  asInteger (A,X) = 1
  asInteger (A,Y) = 2
  asInteger (A,Z) = 3
  asInteger (B,X) = 4
  asInteger (B,Y) = 5
  asInteger (B,Z) = 6
  asInteger (C,X) = 7
  asInteger (C,Y) = 8
  asInteger (C,Z) = 9

  embeddingRange = [1..9]

specification :: Specification
specification =
  readInput p pos AssumeValid <>
  readInput p pos AssumeValid <>
  branch (currentValue p .==. embeddedLit (B,Z))
    (writeOutput [ resultOf $ valueBefore 1 p ])
    (readInput p xPos UntilValid <>
    writeOutput [ resultOf $ currentValue p .==. valueBefore 2 p ]
    )
  where
    p = embeddedVar @Pos "p"
    pos = complete
    xPos = embedFromList [(x, X) | x <- [A, B, C]]

program :: MonadTeletype io => io ()
program = do
  pos1 <- readPosAlt
  pos2 <- readLn @_ @Pos
  if pos2 == (B,Z)
    then print pos1
    else do
      pos3 <- readXPos
      print (pos1 == pos3)

readPosAlt :: MonadTeletype io => io Pos
readPosAlt = do
  o <- getChar
  col <- getChar
  k <- getChar
  row <- getChar
  c <- getChar
  nl <- getChar
  if [o,k,c,nl] == "(,)\n"
    then
      let
        f =
          case col of
            'A' -> pure . (A,)
            'B' -> pure . (B,)
            'C' -> pure . (C,)
            _ -> error "no read"
      in case row of
        'X' -> f X
        'Y' -> f Y
        'Z' -> f Z
        _ -> error "no read"
    else error "no read"

readXPos :: MonadTeletype io => io Pos
readXPos = do
  (c,r) <- readLn
  if r == X then pure (c,r) else readXPos

spec :: Spec
spec = do
  context "value embedding" $ do
    describe "taskCheck program specification" $
      it "is success" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True

    describe "random testing" $ do
      it "works as expected" $
        (Random.taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True

    describe "static test-suite generation" $
      it "works for embedded values" $
        (((\is -> taskCheckOn is program specification) <$> generateStaticTestSuite stdArgs specification) <&> isSuccess) `shouldReturn` True
