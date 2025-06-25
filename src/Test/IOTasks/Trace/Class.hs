{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.IOTasks.Trace.Class where
import Data.Monoid (Endo(..))

class Monoid t => Trace t where
  progReadChar :: Char -> t -> t
  progReadLine :: String -> t -> t
  progReadLine s = appEndo $ foldMap (Endo . progReadChar) (s <> "\n")

  progWriteChar :: Char -> t -> t
  progWriteChar c = progWriteStr [c]
  progWriteStr :: String -> t -> t
  progWriteStr = appEndo . foldMap (Endo . progWriteChar)
  progWriteLine :: String -> t -> t
  progWriteLine c = progWriteStr $ c <> "\n"

  outOfInputs :: t
  terminate :: t

  inputSequence :: t -> [Line]

type Line = String

singleAction :: Trace t => (t -> t) -> t
singleAction = ($ terminate)

class Trace (t a) => GeneralizedTrace t a where
  genProgWrite :: a -> t a -> t a
  genProgWriteLine :: a -> t a -> t a


