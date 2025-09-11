{-# LANGUAGE RecordWildCards #-}
module Test.IOTasks.UnitTests.Generated
( checkAbstract, checkAbstractWith
, checkAbstractOutcome, checkAbstractWithOutcome
, checkConstrained, checkConstrainedWith
, checkConstrainedOutcome, checkConstrainedWithOutcome
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

checkAbstract :: IOrep () -> [AbstractTrace] -> IO ()
checkAbstract = checkAbstractWith stdArgs

checkAbstractOutcome :: IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkAbstractOutcome = checkAbstractWithOutcome stdArgs

checkAbstractWith :: Args -> IOrep () -> [AbstractTrace] -> IO ()
checkAbstractWith args p = void . checkAbstractWithOutcome args p

checkAbstractWithOutcome :: Args -> IOrep () -> [AbstractTrace] -> IO CoreOutcome
checkAbstractWithOutcome Args{..} p ts = do
  suite <- generate (concat <$> mapM (vectorOf testsPerTrace . generateTrace (Size inputRange sequenceLength)) ts)
  checkGeneralizedWithOutcome (Simple.Args terminalOutput feedbackStyle) p suite

checkConstrained :: IOrep () -> [ConstrainedTrace] -> IO ()
checkConstrained = checkConstrainedWith stdArgs

checkConstrainedOutcome :: IOrep () -> [ConstrainedTrace] -> IO CoreOutcome
checkConstrainedOutcome = checkConstrainedWithOutcome stdArgs

checkConstrainedWith :: Args -> IOrep () -> [ConstrainedTrace] -> IO ()
checkConstrainedWith args p = void . checkConstrainedWithOutcome args p

checkConstrainedWithOutcome :: Args -> IOrep () -> [ConstrainedTrace] -> IO CoreOutcome
checkConstrainedWithOutcome Args{..} p ts = do
  suite <- concat <$> mapM (replicateM testsPerTrace . solveTrace (Size inputRange sequenceLength) 1000) ts
  checkGeneralizedWithOutcome (Simple.Args terminalOutput feedbackStyle) p suite
