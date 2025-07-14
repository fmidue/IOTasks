{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.Trace.DirectTrace where

import Test.IOTasks.Trace.Class
import Test.IOTasks.Trace.NormalizedTrace (OptFlag (..))
import Test.IOTasks.Internal.OutputPattern as Pattern
import Test.IOTasks.Trace.PrettyPrint

import Data.Bifunctor (second)
import Data.Set (Set)
import qualified Data.Set as Set

import Text.PrettyPrint as PP hiding ((<>))

data DirectTrace a
  = ReadStr String (DirectTrace a)
  | Write a (DirectTrace a)
  | Terminate
  | OutOfInputs
  deriving (Eq,Show)

isTerminatingDirectTrace :: DirectTrace a -> Bool
isTerminatingDirectTrace (ReadStr _ t) = isTerminatingDirectTrace t
isTerminatingDirectTrace (Write _ t) = isTerminatingDirectTrace t
isTerminatingDirectTrace Terminate = True
isTerminatingDirectTrace OutOfInputs = False

instance Semigroup (DirectTrace a) where
  ReadStr s t <> y = ReadStr s (t <> y)
  Write s t <> y = Write s (t <> y)
  Terminate <> y = y
  OutOfInputs <> _ = OutOfInputs

instance Monoid (DirectTrace a) where
  mempty = Terminate

instance Trace (DirectTrace String) where
  progReadChar = progReadChar'
  progReadLine = progReadLine'

  progWriteStr = progWrite'

  terminate = terminate'
  outOfInputs = outOfInputs'

  inputSequence = inputSequence'

instance Trace (DirectTrace (OptFlag, Set (OutputPattern 'TraceP))) where
  progReadChar = progReadChar'
  progReadLine = progReadLine'

  progWriteStr = progWrite' . liftString

  terminate = terminate'
  outOfInputs = outOfInputs'

  inputSequence = inputSequence'

instance GeneralizedTrace DirectTrace (OptFlag, Set (OutputPattern 'TraceP)) where
  genProgWrite = progWrite'
  genProgWriteLine = progWrite' . second (Set.map (<> Pattern.text "\n"))

liftString :: String -> (OptFlag, Set (OutputPattern 'TraceP))
liftString = (Mandatory,) . Set.singleton . Pattern.text

progReadChar' :: Char -> DirectTrace a -> DirectTrace a
progReadChar' c = ReadStr [c]

progReadLine' :: String -> DirectTrace a -> DirectTrace a
progReadLine' = ReadStr . (<> "\n")

progWrite' :: a -> DirectTrace a -> DirectTrace a
progWrite' = Write

terminate' :: DirectTrace a
terminate' = Terminate

outOfInputs' :: DirectTrace a
outOfInputs' = OutOfInputs

inputSequence' :: DirectTrace a -> [String]
inputSequence' (ReadStr s t) = s: inputSequence' t
inputSequence' (Write _ t) = inputSequence' t
inputSequence' Terminate = []
inputSequence' OutOfInputs = []

instance PrettyPrintWrite a => PrettyPrintTrace (DirectTrace a) a where
  printDeep simple printA f (ReadStr s t) = f ("?" <> PP.text (show s)) (printDeep simple printA f t)
  printDeep simple printA f (Write s t) = f (printA s) (printDeep simple printA f t)
  printDeep _ _ _ Terminate = "stop"
  printDeep _ _ _ OutOfInputs = "excess-input-command"

  defaultConcat = (PP.<+>)
