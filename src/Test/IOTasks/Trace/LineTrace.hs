{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Test.IOTasks.Trace.LineTrace (
  LineTrace,
  pattern ReadLine, pattern WriteLine, pattern LTerminate, pattern LOutOfInputs,
  isTerminatingLineTrace,
  printLineTrace, printLineTrace',
  ) where

import Test.IOTasks.Trace.Class

import Text.PrettyPrint hiding ((<>))

data LineTrace
  = ReadLine' String LineTrace
  | WriteLine' String LineTrace
  | Terminate
  | OutOfInputs
  deriving Eq

instance Show LineTrace where
  show = show'
    where
      show' (ReadLine' s t) = "progReadLine " <> show s <> addDollar t <> show t
      show' (WriteLine' s t) = "progWriteLine " <> show s <> addDollar t <> show t
      show' Terminate = "termintate"
      show' OutOfInputs = "outOfInputs"

      addDollar ReadLine'{} = " $ "
      addDollar WriteLine'{} = " $ "
      addDollar Terminate = " "
      addDollar OutOfInputs = " "

instance Semigroup LineTrace where
  Terminate <> y = y
  OutOfInputs <> _ = OutOfInputs
  ReadLine' s t <> y = progReadLine s (t <> y)
  WriteLine' s t <> y = progWriteLine s (t <> y)

instance Monoid LineTrace where
  mempty = terminate

instance Trace LineTrace where
  progReadChar :: Char -> LineTrace -> LineTrace
  progReadChar '\n' t              = ReadLine' "" t
  progReadChar c    (ReadLine' s t) = ReadLine' (c:s) t
  progReadChar c    t              = ReadLine' [c] t -- this will insert extra line breaks (by design)

  progWriteStr :: String -> LineTrace -> LineTrace
  progWriteStr s t | last s == '\n' = progWriteLine (init s) t
  progWriteStr s (WriteLine' s' t) = foldr WriteLine' (WriteLine' (last ls <> s') t) (init ls)
    where ls = lines s
  progWriteStr s t = WriteLine' s t -- this will insert extra line breaks (by design)

  progWriteLine :: String -> LineTrace -> LineTrace
  progWriteLine s t = foldr WriteLine' t (lines $ s <> "\n")

  terminate :: LineTrace
  terminate = Terminate

  outOfInputs :: LineTrace
  outOfInputs = OutOfInputs

  inputSequence :: LineTrace -> [Line]
  inputSequence (ReadLine' s t) = s : inputSequence t
  inputSequence (WriteLine' _ t) = inputSequence t
  inputSequence Terminate = []
  inputSequence OutOfInputs = []

isTerminatingLineTrace :: LineTrace -> Bool
isTerminatingLineTrace (ReadLine' _ t) = isTerminatingLineTrace t
isTerminatingLineTrace (WriteLine' _ t) = isTerminatingLineTrace t
isTerminatingLineTrace Terminate = True
isTerminatingLineTrace OutOfInputs = False

printLineTrace :: LineTrace -> Doc
printLineTrace = printLineTrace' ($+$)

printLineTrace' :: (Doc -> Doc -> Doc) -> LineTrace -> Doc
printLineTrace' f (ReadLine' s t) = f ("?" <> text (show s)) (printLineTrace' f t)
printLineTrace' f (WriteLine' s t) = f ("!" <> text (show s)) (printLineTrace' f t)
printLineTrace' _ Terminate = "stop"
printLineTrace' _ OutOfInputs = "excess-input-command"

{-# COMPLETE  ReadLine, WriteLine, LTerminate, LOutOfInputs #-}
pattern ReadLine :: String -> LineTrace -> LineTrace
pattern ReadLine s t <- ReadLine' s t
pattern WriteLine :: String -> LineTrace -> LineTrace
pattern WriteLine s t <- WriteLine' s t
pattern LTerminate :: LineTrace
pattern LTerminate <- Terminate
pattern LOutOfInputs :: LineTrace
pattern LOutOfInputs <- OutOfInputs
