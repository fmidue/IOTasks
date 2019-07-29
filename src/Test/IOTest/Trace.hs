module Test.IOTest.Trace where

import Test.IOTest.Language
import Test.IOTest.Utils
import Test.IOTest.Internal.Trace
import Test.IOTest.Internal.TraceSet

import Test.QuickCheck.Gen
import Test.QuickCheck.Random


generateTrace :: Int -> Specification -> NTrace
generateTrace = head ... generateTraces 1

generateTraces :: Int -> Int -> Specification -> [NTrace]
generateTraces len seed spec = unGen (vectorOf len $ traceGen spec) (mkQCGen seed) seed
