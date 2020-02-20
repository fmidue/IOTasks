{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTasks.IOProperty (
  IOTasksable(..),
  accept,
  matchesTrace,
  MatchResult(..),
) where

import Test.IOTasks.IOrep (IOrep, runProgram)
import Test.IOTasks.Specification
import Test.IOTasks.Trace
import Test.IOTasks.TraceSet
import Test.IOTasks.Environment
import Test.IOTasks.Term (SemTerm(..), TermVars(..))
import Test.IOTasks.Pattern
import Test.IOTasks.ValueSet

import Data.Coerce
import Data.Maybe (fromMaybe)

import Test.QuickCheck
import Text.PrettyPrint.HughesPJClass hiding ((<>))

class IOTasksable a b where
  fulfills :: a -> b -> Property
  neverFulfills :: a -> b -> Property
  fulfillsNotFor :: [String] -> a -> b -> Property

instance (SemTerm t, TermVars t) => IOTasksable (IOrep ()) (Specification t) where
  fulfills prog spec = specProperty True spec prog
  neverFulfills prog spec = specProperty False spec prog
  fulfillsNotFor ins prog spec =
    let trace = runProgram ins prog
    in property . not $ spec `accept` trace

instance (Show b, Arbitrary b, IOTasksable a' b', Coercible b a) => IOTasksable (a -> a') (b -> b') where
  fulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `fulfills` g x)
  neverFulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `neverFulfills` g x)
  fulfillsNotFor ins f g = forAllShrink arbitrary shrink (\x -> fulfillsNotFor ins (f (coerce x)) (g x))

-- target represents the expected outcome of checking the property,
-- i.e. the property is satisfied iff for all inputs the performed check returns target
specProperty :: (SemTerm t, TermVars t) => Bool -> Specification t -> IOrep () -> Property
specProperty target spec program =
  let gen = traceGen spec
      prop tg = case program `matchesTrace` tg of
          (MatchSuccessfull,(len, trace)) -> addCounterexample MatchSuccessfull (len,trace) target
          (err, (len, trace)) -> addCounterexample err (len,trace) $ not target
  in forAllShow gen (render . printGenNTraceInfo) prop

matchesTrace :: IOrep () -> GeneralizedTrace -> (MatchResult, (Int, OrdinaryTrace))
matchesTrace program tg =
  let inputs = inputsN tg
      len = length inputs
      trace = runProgram inputs program
      result = trace `isCoveredBy` tg
  in (result,(len,trace))

addCounterexample :: Testable prop => MatchResult -> (Int,OrdinaryTrace) -> prop -> Property
addCounterexample res (n,trace) = counterexample . render $
  hang (text "Actual run:") 4
    (ppTrace n trace)
  $$ hang (text "Error:") 4 errorMsg
  where errorMsg = case res of
          MatchSuccessfull -> text "Expected error, but matching succeeded"
          err -> ppResult err

accept :: (SemTerm t, TermVars t) => Specification t -> OrdinaryTrace -> Bool
accept s@(Spec as) t = accept' as kI t (freshEnvironment (specVars s)) where
  kI Exit _    _ = error "loop exit marker on toplevel"
  kI End  Stop _ = True
  kI End  _    _ = False

accept' :: SemTerm t => [Action t] -> (Cont -> OrdinaryTrace -> Environment -> Bool) -> OrdinaryTrace -> Environment -> Bool
accept' (ReadInput x ty : s') k (ProgRead v t') env =
  let val = valueFromString ty v
      env' = fromMaybe (error "accept: environment update failed") (storeValue x val env)
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
