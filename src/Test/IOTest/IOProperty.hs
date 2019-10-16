{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.IOProperty (
  IOProperty,
  fulfills,
  fulfillsNot,
) where

import Test.IOTest.IOrep (IOrep, runProgram)
import Test.IOTest.Specification
import Test.IOTest.Trace
import Test.IOTest.TraceSet

import Control.Arrow ((&&&))
import Data.Coerce

import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass

data IOProperty prog spec = Fulfills prog spec | FulfillsNot prog spec

fulfills :: a -> b -> IOProperty a b
fulfills = Fulfills
fulfillsNot :: a -> b -> IOProperty a b
fulfillsNot = FulfillsNot

instance Testable (IOProperty (IOrep ()) Specification) where
  property (prog `Fulfills` spec) = specProperty spec prog
  property (prog `FulfillsNot` spec) = expectFailure $ specProperty spec prog

instance (Show b, Arbitrary b, Testable (IOProperty a' b'), Coercible b a) => Testable (IOProperty (a -> a') (b -> b')) where
  property (Fulfills f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `Fulfills` g x)
  property (FulfillsNot f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `FulfillsNot` g x)

specProperty :: Specification -> IOrep () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAllShow gen (render . printNTraceInfo) prop

testTrace :: (NTrace, [String]) -> IOrep () -> Property
testTrace (tg,ins) p =
  let trace = runProgram ins p
      normalized = normalize trace
  in case normalized `isCoveredBy` tg of
      MatchSuccessfull -> property True
      err -> counterexample (render $
        hang (text "Actual run:") 4
          (pPrint trace)
       $$ hang (text "Error:") 4
         (ppResult err) )
       False

  --in _ -- counterexample (msg ++ "\n  program trace: " ++ show normalized) result
