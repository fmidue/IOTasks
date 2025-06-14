{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Test.IOTasks.Testing (
  taskCheck, taskCheckWith, taskCheckOutcome, taskCheckWithOutcome,
  Args(..), stdArgs,
  FeedbackStyle(..), TraceStyle(..), defaultFeedback,
  Outcome(..), CoreOutcome(..), OutcomeHints(..),
  ExpectedRun, ActualRun,
  isSuccess, isFailure,
  overflowWarnings,
  printOutcomeWith, pPrintOutcomeHints,
  -- | = pre-computed test suites
  taskCheckOn, generateStaticTestSuite,
  Inputs,
  ) where

import Test.IOTasks.IOrep (IOrep, Line, runProgram)
import Test.IOTasks.Specification
import Test.IOTasks.Constraints
import Test.IOTasks.Trace
import Test.IOTasks.Z3
import Test.IOTasks.Overflow
import Test.IOTasks.Internal.Output
import Test.IOTasks.FeedbackStyle

import Control.Concurrent.STM
import Control.Monad (when, forM, replicateM)

import Data.List (sortOn)
import Data.Functor (void)
import Data.Bifunctor (first, Bifunctor (second))

import Text.PrettyPrint hiding ((<>))
import Control.Concurrent.Async
import System.IO
import Test.QuickCheck (generate)

taskCheck :: IOrep () -> Specification -> IO ()
taskCheck = taskCheckWith stdArgs

data Args
  = Args
    -- | maximum number of iteration unfoldings when searching for satisfiable paths.
    --   (indirectly controls maximum path length)
  { maxIterationUnfold :: Int
    -- | maximum absolute value of randomly generated candidates for input values (the solver might find bigger solutions)
  , inputRange :: Integer
    -- | solver timeout in milliseconds
  , solverTimeout :: Int
    -- | maximum number of solver timeouts before giving up
  , maxTimeouts :: Int
    -- | minimum number of tests generated per path
    --   (when using 'InputMode' 'UntilValid' more test will be performed)
  , testsPerPath :: Int
    -- | maximum number of negative inputs per path (for 'InputMode' 'UntilValid')
  , maxNegative :: Int
    -- | print extra information
  , terminalOutput :: Bool
    -- | feedback formating
  , feedbackStyle :: FeedbackStyle
    -- | check that intermediate results do not cause Int overflows,
    --   including parts of 'OutputTerms' when possible (best-effort, no guarantees on completeness)
  , avoidOverflows :: Bool
    -- | maximum length of string values in the backend solver.
    --   Smaller values may speed up test case generation, at the risk of not finding some satisfiable paths
  , solverMaxSeqLength :: Int
  }

stdArgs :: Args
stdArgs = Args
  { maxIterationUnfold = 25
  , inputRange = 100
  , solverTimeout = 1000 -- 1 sec
  , maxTimeouts = 3
  , testsPerPath = 5
  , maxNegative = 5
  , terminalOutput = True
  , feedbackStyle = defaultFeedback
  , avoidOverflows = True
  , solverMaxSeqLength = 25
  }

taskCheckWith :: Args -> IOrep () -> Specification -> IO ()
taskCheckWith args p s = void $ taskCheckWithOutcome args p s

taskCheckOutcome :: IOrep () -> Specification -> IO Outcome
taskCheckOutcome = taskCheckWithOutcome stdArgs

type SatPaths = Int
type NumberOfInputs = Int
type Timeouts = Int
type Overflows = Int
type PartialTimeout = Bool

taskCheckWithOutcome :: Args -> IOrep () -> Specification -> IO Outcome
taskCheckWithOutcome Args{..} prog spec = do
  output <- newOutput stdout
  q <- atomically newTQueue
  nVar <- newTVarIO Nothing

  (_,(coreOut,satPaths,nInputs,timeouts,overflows)) <- concurrently
    (satPathsQ nVar solverTimeout (constraintTree spec) maxIterationUnfold solverMaxSeqLength avoidOverflows q)
    (testProcedure output nVar q)

  let out = Outcome coreOut (if overflows == 0 then NoHints else OverflowHint overflows)
  --
  when terminalOutput $ do
    putLnP output $ unwords
      ["generated", show nInputs,"input", useSingularIf (nInputs == 1) "sequence" "sequences", "covering", show satPaths, "satisfiable", useSingularIf (nInputs == 1) "path" "paths"]
    when (timeouts > 0) $
      putLnP output $ unwords ["---",show timeouts, useSingularIf (timeouts == 1) "path" "paths", "timed out"]
  --
  printP output $ printOutcomeWith feedbackStyle out
  pure out

  where
    testProcedure :: Output -> TVar (Maybe Int) -> TQueue (Maybe SimplePath) -> IO (CoreOutcome,SatPaths,NumberOfInputs,Timeouts,Overflows)
    testProcedure output nVar q = mainLoop (0,0,0,0) Nothing
      where
        mainLoop :: (SatPaths,NumberOfInputs,Timeout,Overflows) -> Maybe CoreOutcome -> IO (CoreOutcome,SatPaths,NumberOfInputs,Timeouts,Overflows)
        mainLoop (nP,n,t,o) mFailure | t > maxTimeouts =
          case mFailure of
            (Just failure) -> pure (failure,nP,n,t,o)
            Nothing -> pure (GaveUp,nP,n,t,o)
        mainLoop (nP,n,t,o) mFailure = do
          next <- atomically $ readTQueue q
          currentMaxPathLength <- readTVarIO nVar
          case next of
            Nothing -> case mFailure of
              Just failure -> pure (failure,nP,n,t,o)
              Nothing -> pure (Success n,nP,n,t,o)
            Just p
              | maybe False (pathDepth (completePath [] p) >) currentMaxPathLength -> mainLoop (nP,n,t,o) mFailure -- skip this path
              | otherwise -> do
                (outPlain,n',oPlain) <- testPlain n p
                case outPlain of
                  PathFailure{} -> handleFailure (nP,n',t,o+oPlain) outPlain mFailure
                  PathTimeout -> handleTimeout (n' > n) (nP,n',t,o+oPlain) mFailure
                  PathSuccess -> do
                    if canBeInjected p
                      then do
                        (outInject,n'',oInject) <- testInjected n' p
                        case outInject of
                          PathFailure{} -> handleFailure (nP,n'',t,o+oPlain+oInject) outInject mFailure
                          PathTimeout -> handleTimeout (n'' > n') (nP,n'',t,o+oPlain+oInject) mFailure
                          PathSuccess -> mainLoop (nP+1,n'',t,o+oPlain+oInject) mFailure
                      else mainLoop (nP+1,n',t,o+oPlain) mFailure

        handleFailure :: (SatPaths,NumberOfInputs,Timeout,Overflows) -> PathOutcome -> Maybe CoreOutcome -> IO (CoreOutcome,SatPaths,NumberOfInputs,Timeouts,Overflows)
        handleFailure (nP,n,t,o) (PathFailure i et at r) mFailure = do
          if maybe True (\case {Failure j _ _ _ -> length i < length j; _ -> error "impossible"}) mFailure -- there might be paths in the queue that are longer than a found counterexample
            then do
              atomically $ writeTVar nVar (Just $ length i - 1)
              mainLoop (nP+1,n,t,o) (Just $ Failure i et at r)
            else
              mainLoop (nP+1,n,t,o) mFailure
        handleFailure _ _ _ = error "handleFailure: impossible"

        handleTimeout :: PartialTimeout -> (SatPaths,NumberOfInputs,Timeout,Overflows) -> Maybe CoreOutcome -> IO (CoreOutcome,SatPaths,NumberOfInputs,Timeouts,Overflows)
        handleTimeout True (nP,n,t,o) = mainLoop (nP+1,n,t,o)
        handleTimeout False (nP,n,t,o) = mainLoop (nP,n,t+1,o)

        testPlain :: NumberOfInputs -> SimplePath -> IO (PathOutcome,NumberOfInputs,Overflows)
        testPlain n p = pathLoop (n,0) $ replicate testsPerPath $ completePath [] p

        testInjected :: NumberOfInputs -> SimplePath -> IO (PathOutcome,NumberOfInputs,Overflows)
        testInjected n p = do
          injector <- mkPathInjector p solverTimeout solverMaxSeqLength avoidOverflows
          ps <- replicateM testsPerPath (generate $ injectNegatives injector maxNegative)
          (out,n',o) <- pathLoop (n,0) ps
          case out of
            PathFailure{} -> do
              smaller <- sequence [ generate $ injectNegatives injector m | m <- [1..maxNegative] ]
              (outShrink,n'',o') <- pathLoop (n',o) smaller
              pure $ case outShrink of
                PathFailure{} -> (outShrink,n'',o')
                _ -> (out,n'',o')
            _ -> pure (out,n',o)

        pathLoop :: (NumberOfInputs,Overflows) -> [Path] -> IO (PathOutcome,NumberOfInputs,Overflows)
        pathLoop (n,o) [] = pure (PathSuccess,n,o)
        pathLoop (n,o) (p:ps) = do
          nextInput <- findPathInput solverTimeout p inputRange solverMaxSeqLength avoidOverflows
          case nextInput of
            NotSAT -> reportUnsat >> pathLoop (n,o) ps
            Timeout -> pure (PathTimeout,n,o)
            SAT input -> do
              reportTestExecution n
              let
                (out,overflow) = second (== OverflowOccurred) $ testInput input
                o' = if overflow then o+1 else o
              when overflow reportOverflow
              case out of
                PathSuccess -> pathLoop (n+1,o') ps
                out@PathFailure{}  -> do
                  reportCounterexample (length input)
                  pure (out,n,o')
                PathTimeout -> error "does not happen"

        testInput :: Inputs -> (PathOutcome,OverflowWarning)
        testInput input =
          let
            (specTrace,warn) = first normalizedTrace $ runSpecification spec input
            progTrace = normalizedTrace $ runProgram prog input
          in case specTrace `covers` progTrace of
            result | isSuccessfulMatch result -> (PathSuccess,warn)
            failure -> (PathFailure input specTrace progTrace failure, warn)

        reportTestExecution :: Int -> IO ()
        reportTestExecution n = when terminalOutput (putT output (concat ["(",show n," tests)"]) >> oFlush output)

        reportCounterexample :: Int -> IO ()
        reportCounterexample len = when terminalOutput $ putLnP output $ unwords ["found counterexample of length",show len]

        reportOverflow :: IO ()
        reportOverflow = when terminalOutput $ putLnP output "Overflow of Int range detected."

        reportUnsat :: IO ()
        reportUnsat = when terminalOutput $ putLnP output "unexpected unsat result in test case generation, maybe some value set is (conditionally) uninhabited"

type Inputs = [Line]
type ExpectedRun = NTrace
type ActualRun = NTrace

data Outcome = Outcome CoreOutcome OutcomeHints
  deriving (Eq, Show)

data CoreOutcome = Success Int | Failure Inputs ExpectedRun ActualRun MatchResult | GaveUp
  deriving (Eq, Show)

data OutcomeHints = NoHints | OverflowHint Int
  deriving (Eq, Show)

instance Semigroup Outcome where
  (Outcome f@Failure{} h) <> _ = Outcome f h
  (Outcome GaveUp h) <> _ = Outcome GaveUp h
  (Outcome cx hx) <> (Outcome cy hy) = Outcome (cx <> cy) (hx <> hy)
instance Monoid Outcome where
  mempty = Outcome mempty mempty

instance Monoid CoreOutcome where
  mempty = Success 0
instance Semigroup CoreOutcome where
  Success x <> Success y = Success $ x + y
  Success _ <> f@Failure{} = f
  Success _ <> GaveUp = GaveUp
  f@Failure{} <> _ = f
  GaveUp <> _ = GaveUp

instance Monoid OutcomeHints where
  mempty = NoHints
instance Semigroup OutcomeHints where
  NoHints <> y = y
  x <> NoHints = x
  OverflowHint x <> OverflowHint y = OverflowHint $ x + y

isSuccess :: Outcome -> Bool
isSuccess (Outcome Success{} _) = True
isSuccess _ = False

isFailure :: Outcome -> Bool
isFailure (Outcome Failure{} _) = True
isFailure _ = False

overflowWarnings :: Outcome -> Int
overflowWarnings (Outcome _ NoHints) = 0
overflowWarnings (Outcome _ (OverflowHint n)) = n

data PathOutcome = PathSuccess | PathTimeout | PathFailure Inputs ExpectedRun ActualRun MatchResult
  deriving (Eq,Show)

printOutcomeWith :: FeedbackStyle -> Outcome -> Doc
printOutcomeWith FeedbackStyle{..}
  | simplifyFeedback = pPrintOutcomeSimple layoutHow
  | otherwise = pPrintOutcome layoutHow
  where
    layoutHow = case traceStyle of
      HorizontalTrace -> (<+>)
      VerticalTrace -> ($+$)

pPrintOutcome :: (Doc -> Doc -> Doc) -> Outcome -> Doc
pPrintOutcome f (Outcome core hints) = pPrintOutcomeHints hints $+$ pPrintCoreOutcome False f core

pPrintOutcomeSimple :: (Doc -> Doc -> Doc) -> Outcome -> Doc
pPrintOutcomeSimple f (Outcome core hints) = pPrintOutcomeHints hints $+$ pPrintCoreOutcome True f core

pPrintCoreOutcome :: Bool -> (Doc -> Doc -> Doc) -> CoreOutcome -> Doc
pPrintCoreOutcome _ _ (Success n) = text $ unwords ["+++ OK, passed",show n,useSingularIf (n==1) "test." "tests."]
pPrintCoreOutcome _ _ GaveUp = text "*** Gave up!"
pPrintCoreOutcome simple f (Failure is et at r) = vcat
  [ text "*** Failure"
  , text ("Input sequence: "++ showInputs is)
  , text ("Expected run" ++ when simple " (simplified)" ++ ":") <+> showTraceHow et
  , text "Actual run:" <+> showTraceNSimple' f at
  , text "Error:"
  , nest 2 (pPrintResult f r)
  ]
  where
    when b str = if b then str else ""
    (pPrintResult, showTraceHow)
      | simple = (pPrintMatchResultSimple,showTraceNSimple' f)
      | otherwise = (pPrintMatchResult,showTraceN' f)

showInputs :: Inputs -> String
showInputs = unwords . map ('?':)

pPrintOutcomeHints :: OutcomeHints -> Doc
pPrintOutcomeHints NoHints = mempty
pPrintOutcomeHints (OverflowHint n) = text $ unwords [show n,"overflows of Int range were detected during testing"]

-- static test suite generation
taskCheckOn :: [Inputs] -> IOrep () -> Specification -> Outcome
taskCheckOn i p s = uncurry Outcome (go 0 0 i p s) where
  go n o [] _ _ = (Success n, overflowHint o)
  go n o (i:is) prog spec =
    let
      (specTrace,warn) = first normalizedTrace $ runSpecification spec i
      progTrace = normalizedTrace $ runProgram prog i
      o' = if warn == OverflowOccurred then o+1 else o
    in case specTrace `covers` progTrace of
      result | isSuccessfulMatch result -> go (n+1) o' is prog spec
      failure -> (Failure i specTrace progTrace failure, overflowHint o')

  overflowHint 0 = NoHints
  overflowHint n = OverflowHint n

generateStaticTestSuite :: Args -> Specification -> IO [Inputs]
generateStaticTestSuite Args{..} spec = do
  let simple = simplePaths maxIterationUnfold $ constraintTree spec
  full <- concat <$> forM simple (\p -> do
    injector <- mkPathInjector p solverTimeout solverMaxSeqLength avoidOverflows
    replicateM testsPerPath (generate $ injectNegatives injector maxNegative))
  let sortedPaths = sortOn pathDepth full
  catSATs <$> forM sortedPaths (\p -> findPathInput solverTimeout p inputRange solverMaxSeqLength avoidOverflows)

catSATs :: [SatResult a] -> [a]
catSATs [] = []
catSATs (SAT x : xs) = x : catSATs xs
catSATs (_:xs) = catSATs xs

useSingularIf :: Bool -> String -> String -> String
useSingularIf p singular plural
  | p = singular
  | otherwise = plural
