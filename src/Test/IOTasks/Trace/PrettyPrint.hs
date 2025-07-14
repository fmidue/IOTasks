{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Test.IOTasks.Trace.PrettyPrint where

import Text.PrettyPrint as PP hiding ((<>))

type SimpleRead = Bool
type SimpleWrite = Bool

class PrettyPrintWrite a => PrettyPrintTrace t a | t -> a where
  printDeep :: SimpleRead -> (a -> Doc) -> (Doc -> Doc -> Doc) -> t -> Doc
  defaultConcat :: Doc -> Doc -> Doc

class PrettyPrintWrite a where
  printA :: SimpleWrite -> a -> Doc

instance PrettyPrintWrite String where
  printA _ s = "!" <> PP.text (show s)

printTrace :: forall t a. PrettyPrintTrace t a => t -> Doc
printTrace = printTrace' @t (defaultConcat @t)

printTrace' :: forall t a. PrettyPrintTrace t a => (Doc -> Doc -> Doc) -> t -> Doc
printTrace' = printDeep False (printA @a False)

printTraceSimple :: forall t a. PrettyPrintTrace t a => t -> Doc
printTraceSimple = printTraceSimple' @t (defaultConcat @t)

printTraceSimple' :: forall t a. PrettyPrintTrace t a => (Doc -> Doc -> Doc) -> t -> Doc
printTraceSimple' = printDeep True (printA @a True)

printTraceHead :: forall t a. PrettyPrintTrace t a => t -> Doc
printTraceHead = printTrace' @t const

printTraceHeadSimple :: forall t a. PrettyPrintTrace t a => t -> Doc
printTraceHeadSimple = printTraceSimple' @t const
