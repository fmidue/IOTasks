{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.IOProperty (
  fulfills,
  neverFulfills,
  fulfillsNotFor,
  accept,
) where

import Test.IOTest.IOrep (IOrep, runProgram)
import Test.IOTest.Specification
import Test.IOTest.Trace
import Test.IOTest.TraceSet
import Test.IOTest.Environment
import Test.IOTest.Term
import Test.IOTest.Pattern
import Test.IOTest.ValueSet

import Control.Arrow ((&&&))

import Data.Coerce
import Data.Maybe (fromMaybe)

import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding ((<>))

class IOTestable a b where
  fulfills :: a -> b -> Property
  neverFulfills :: a -> b -> Property
  fulfillsNotFor :: [String] -> a -> b -> Property

instance IOTestable (IOrep ()) Specification where
  fulfills prog spec = specProperty True spec prog
  neverFulfills prog spec = specProperty False spec prog
  fulfillsNotFor ins prog spec =
    case runProgram ins prog of
      Nothing -> property True
      Just trace -> property . not $ spec `accept` trace

instance (Show b, Arbitrary b, IOTestable a' b', Coercible b a) => IOTestable (a -> a') (b -> b') where
  fulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `fulfills` g x)
  neverFulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `neverFulfills` g x)
  fulfillsNotFor ins f g = forAllShrink arbitrary shrink (\x -> fulfillsNotFor ins (f (coerce x)) (g x))

-- target represents the expected outcome of checking the property,
-- i.e. the property is satisfied iff for all inputs the performed check returns target
specProperty :: Bool -> Specification -> IOrep () -> Property
specProperty target spec program =
  let gen = traceGen spec
      prop t = testTrace target ((id &&& inputsN) t) program
  in forAllShow gen (render . printGenNTraceInfo) prop

testTrace :: Bool -> (GeneralizedTrace, [String]) -> IOrep () -> Property
testTrace target (tg,ins) p =
  case runProgram ins p of
    Nothing -> counterexample outOfInputsMsg $ property (not target)
    Just trace -> case trace `isCoveredBy` tg of
      MatchSuccessfull -> formatCounterexample MatchSuccessfull trace $ property target
      err -> formatCounterexample err trace $ not target

formatCounterexample :: Testable prop => MatchResult -> OrdinaryTrace -> prop -> Property
formatCounterexample res trace = counterexample . render $
  hang (text "Actual run:") 4
    (pPrint trace)
  $$ hang (text "Error:") 4 errorMsg
  where errorMsg = case res of
          MatchSuccessfull -> text "Expected error, but matching succeeded"
          err -> ppResult err

outOfInputsMsg :: String
outOfInputsMsg = render . text $
  "Could not produce a complete trace for the given input sequence because the "
  ++ "program atempted to read more than the provied ones."

accept :: Specification -> OrdinaryTrace -> Bool
accept s@(Spec as) t = accept' as kI t (freshEnvironment s) where
  kI Exit _    _ = error "loop exit marker on toplevel"
  kI End  Stop _ = True
  kI End  _    _ = False

accept' :: [Action] -> (Cont -> OrdinaryTrace -> Environment -> Bool) -> OrdinaryTrace -> Environment -> Bool
accept' (ReadInput x ty : s') k (ProgRead v t') env =
  let val = valueFromString ty v
      env' = fromMaybe (error "accept: environment update failed") (updateWithValue x val env)
  in containsValue ty val && accept' s' k t' env'
accept' (ReadInput x ty : s') k t env = False
accept' (WriteOutput True ps ts : s') k t env =
  accept' (WriteOutput False ps ts : s') k t env || accept' s' k t env
accept' (WriteOutput False ps ts : s') k (ProgWrite v t') env =
  let vs = (\p -> fillHoles (p,ts) env) <$> ps
  in any (v `isContainedIn`) vs && accept' s' k t' env
accept' (WriteOutput{} : _) _ _ _ = False
accept' (Branch c (Spec s1) (Spec s2) : s') k t env =
  if evalTerm c env
    then accept' (s2 <> s') k t env
    else accept' (s1 <> s') k t env
accept' (TillE (Spec s) : s') k t env =
  let k' End  = accept' s  k'
      k' Exit = accept' s' k
  in accept' s k' t env
accept' (E:_) k t env = k Exit t env
accept' [] k t env = k End t env

data Cont = Exit | End
