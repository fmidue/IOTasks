{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.Trace.PrettyPrint where

import Text.PrettyPrint as PP hiding ((<>))

type SimpleRead = Bool
type SimpleWrite = Bool

class PrettyPrintTrace t where
  printDeep :: SimpleRead -> (forall a. PrettyPrintWrite a => a -> Doc) -> (Doc -> Doc -> Doc) -> t -> Doc
  defaultConcat :: Doc -> Doc -> Doc

class PrettyPrintWrite a where
  printA :: SimpleWrite -> a -> Doc

instance PrettyPrintWrite String where
  printA _ s = "!" <> PP.text (show s)

printTrace :: forall t. PrettyPrintTrace t => t -> Doc
printTrace = printTrace' @t (defaultConcat @t)

printTrace' :: forall t. PrettyPrintTrace t => (Doc -> Doc -> Doc) -> t -> Doc
printTrace' = printDeep False (printA False)

printTraceSimple :: forall t. PrettyPrintTrace t => t -> Doc
printTraceSimple = printTraceSimple' @t (defaultConcat @t)

printTraceSimple' :: forall t. PrettyPrintTrace t => (Doc -> Doc -> Doc) -> t -> Doc
printTraceSimple' = printDeep True (printA True)

printTraceHead :: forall t. PrettyPrintTrace t => t -> Doc
printTraceHead = printTrace' @t const

printTraceHeadSimple :: forall t. PrettyPrintTrace t => t -> Doc
printTraceHeadSimple = printTraceSimple' @t const
