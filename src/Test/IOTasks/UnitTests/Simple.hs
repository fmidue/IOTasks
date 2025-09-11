{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Test.IOTasks.UnitTests.Simple (
  checkSimple, checkSimpleWith,
  checkSimpleOutcome, checkSimpleWithOutcome,
  checkGeneralized, checkGeneralizedWith,
  checkGeneralizedOutcome, checkGeneralizedWithOutcome,
  Args(..), stdArgs,
  SimpleOutcome(..), isSimpleSuccess, isSimpleFailure,
  ) where

import Prelude hiding (putStrLn, print)

import Test.IOTasks.Trace
import Test.IOTasks.Trace.Match

import Control.Monad

import qualified Text.PrettyPrint as PP
import Test.IOTasks.Testing (FeedbackStyle(..), CoreOutcome(..), printOutcomeWith, OutcomeHints(..), Outcome(..), defaultFeedback, Inputs, TraceStyle(..))
import Test.IOTasks.IOrep
import Test.IOTasks.Internal.Output
import Text.PrettyPrint (Doc)

data Args
  = Args
  { -- | print extra information
    terminalOutput :: Bool
    -- | feedback formating
  , feedbackStyle :: FeedbackStyle
  }

stdArgs :: Args
stdArgs = Args
  { terminalOutput = True
  , feedbackStyle = defaultFeedback
  }

data SimpleOutcome where
  Passed :: Int -> SimpleOutcome
  Failed :: (Trace t, PrettyPrintTrace t) => Inputs -> t -> t -> SimpleOutcome

-- TODO: de-duplicate (pPrintCoreOutcome)
printSimpleOutcomeWith :: FeedbackStyle -> SimpleOutcome -> Doc
printSimpleOutcomeWith _ (Passed n) = "+++ OK, passed" PP.<+> PP.text (show n) PP.<+> PP.text "tests"
printSimpleOutcomeWith (FeedbackStyle simple traceStyle) (Failed is expected actual) = PP.vcat
  [ "*** Failure"
  , PP.text ("Input sequence: "++ showInputs is)
  , PP.text ("Expected run" ++ when simple " (simplified)" ++ ":") PP.<+> showTraceHow expected
  , "Actual run:" PP.<+> showTraceHow actual
  ]
  where
    showInputs :: Inputs -> String
    showInputs = unwords . map ('?':)

    when b str = if b then str else ""

    layoutHow = case traceStyle of
      HorizontalTrace -> (PP.<+>)
      VerticalTrace -> (PP.$+$)
    showTraceHow
      | simple =  printTraceSimple' layoutHow
      | otherwise = printTrace' layoutHow

isSimpleSuccess :: SimpleOutcome -> Bool
isSimpleSuccess Passed{} = True
isSimpleSuccess Failed{} = False

isSimpleFailure :: SimpleOutcome -> Bool
isSimpleFailure Passed{} = False
isSimpleFailure Failed{} = True

checkSimple :: (Eq t, Trace t, PrettyPrintTrace t) => IOrep () -> [t] -> IO ()
checkSimple = checkSimpleWith stdArgs

checkSimpleWith :: (Eq t, Trace t, PrettyPrintTrace t) => Args -> IOrep () -> [t] -> IO ()
checkSimpleWith args p ts = void $ checkSimpleWithOutcome args p ts

checkSimpleOutcome :: (Eq t, Trace t, PrettyPrintTrace t) => IOrep () -> [t] -> IO SimpleOutcome
checkSimpleOutcome = checkSimpleWithOutcome stdArgs

checkSimpleWithOutcome :: (Eq t, Trace t, PrettyPrintTrace t) => Args -> IOrep () -> [t] -> IO SimpleOutcome
checkSimpleWithOutcome Args{..} p ts = do
  output <- newOutput stdout
  out <- testRunner output 0 ts
  printP output $ printSimpleOutcomeWith feedbackStyle out
  pure out
  where
    testRunner _ n [] = pure $ Passed n
    testRunner output n (t:ts) = do
      let
        is = inputSequence t
        tp = runProgram p is
      do
        when terminalOutput (putT output (concat ["(",show n," tests)"]) >> oFlush output)
        if t == tp
          then testRunner output (n+1) ts
          else pure $ Failed is t tp

checkGeneralized :: IOrep () -> [NTraceGeneralized] -> IO ()
checkGeneralized = checkGeneralizedWith stdArgs

checkGeneralizedWith :: Args -> IOrep () -> [NTraceGeneralized] -> IO ()
checkGeneralizedWith args p ts = void $ checkGeneralizedWithOutcome args p ts

checkGeneralizedOutcome :: IOrep () -> [NTraceGeneralized] -> IO CoreOutcome
checkGeneralizedOutcome = checkGeneralizedWithOutcome stdArgs

checkGeneralizedWithOutcome :: Args -> IOrep () -> [NTraceGeneralized] -> IO CoreOutcome
checkGeneralizedWithOutcome Args{..} p ts = do
  output <- newOutput stdout
  out <- testRunner output 0 ts
  printP output $ printOutcomeWith feedbackStyle (Outcome out NoHints)
  pure out
  where
    testRunner _ n [] = pure $ Success n
    testRunner output n (testCase:ts) =
      let
        is = inputSequence testCase
        progTrace :: Trace t => t
        progTrace = runProgram p is
      in do
        when terminalOutput (putT output (concat ["(",show n," tests)"]) >> oFlush output)
        case testCase `covers` progTrace of
          MatchSuccessful -> testRunner output (n+1) ts
          m -> pure $ Failure is testCase progTrace m
