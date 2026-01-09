{-# LANGUAGE RecordWildCards #-}
module Test.IOTasks.UnitTests.Generated
-- * Testing with random-search-based instantiation
( checkRandom, checkRandomWith
, checkRandomOutcome, checkRandomWithOutcome
-- * Testing with SMT-solver-based instantiation
, checkConstrained, checkConstrainedWith
, checkConstrainedOutcome, checkConstrainedWithOutcome
-- * Testing parameters
, Args(..), stdArgs,
) where

import Control.Monad (replicateM)

import Test.QuickCheck ( generate, vectorOf )

import Test.IOTasks.IOrep
import Test.IOTasks.Trace.Generated
import Test.IOTasks.UnitTests.Simple (checkGeneralizedWithOutcome )
import qualified Test.IOTasks.UnitTests.Simple as Simple (Args(..))
import Test.IOTasks.ValueSet
import Test.IOTasks.FeedbackStyle
import Test.IOTasks.Testing (CoreOutcome)
import Data.Functor (void)

data Args = Args
    -- | maximum absolute value of randomly generated candidates for input values (the solver might find bigger solutions)
  { inputRange :: Integer
    -- maximum length of string values
  , sequenceLength :: Int
    -- | minimum number of tests generated per given trace
  , testsPerTrace :: Int
      -- | print extra information
  , terminalOutput :: Bool
    -- | feedback formating
  , feedbackStyle :: FeedbackStyle
  }

stdArgs :: Args
stdArgs = Args
  { inputRange = 100
  , sequenceLength = 25
  , testsPerTrace = 5
  , terminalOutput = True
  , feedbackStyle = defaultFeedback
  }

checkRandom :: IOrep () -> [AbstractTrace] -> IO ()
checkRandom = checkRandomWith stdArgs

checkRandomOutcome :: IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkRandomOutcome = checkRandomWithOutcome stdArgs

checkRandomWith :: Args -> IOrep () -> [AbstractTrace] -> IO ()
checkRandomWith args p = void . checkRandomWithOutcome args p

checkRandomWithOutcome :: Args -> IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkRandomWithOutcome Args{..} p ts = do
  suite <- generate (concat <$> mapM (vectorOf testsPerTrace . generateTrace (Size inputRange sequenceLength)) ts)
  checkGeneralizedWithOutcome (Simple.Args terminalOutput feedbackStyle) p suite

checkConstrained :: IOrep () -> [AbstractTrace] -> IO ()
checkConstrained = checkConstrainedWith stdArgs

checkConstrainedOutcome :: IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkConstrainedOutcome = checkConstrainedWithOutcome stdArgs

checkConstrainedWith :: Args -> IOrep () -> [AbstractTrace] -> IO ()
checkConstrainedWith args p = void . checkConstrainedWithOutcome args p

checkConstrainedWithOutcome :: Args -> IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkConstrainedWithOutcome Args{..} p ts = do
  suite <- concat <$> mapM (replicateM testsPerTrace . solveTrace (Size inputRange sequenceLength) 1000) ts
  checkGeneralizedWithOutcome (Simple.Args terminalOutput feedbackStyle) p suite
