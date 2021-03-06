{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Test.IOTasks.IOProperty (
  IOTestable(..),
  CleverIOTestable(..),
  accept,
  matchesTrace,
  MatchResult(..),
) where

import Test.IOTasks.IOrep (IOrep, runProgram)
import Test.IOTasks.Specification
import Test.IOTasks.Trace
import Test.IOTasks.TraceSet
import Data.Environment
import Data.Term (SemTerm(..), VarListTerm(..), PTerm)
import Test.IOTasks.Pattern
import Test.IOTasks.ValueSet
import Test.IOTasks.InputGen

import Data.Coerce
import Data.Maybe (fromMaybe, fromJust)

import Test.QuickCheck
import qualified Text.PrettyPrint.HughesPJClass as PP

class IOTestable a b where
  fulfills :: a -> b -> Property
  neverFulfills :: a -> b -> Property
  fulfillsNotFor :: [String] -> a -> b -> Property

-- prototype for constarint based testing
class IOTestable a b => CleverIOTestable a b where
  fulfillsClever :: a -> b -> Property

instance (SemTerm t (Environment Varname), VarListTerm t Varname) => IOTestable (IOrep ()) (Specification t) where
  fulfills prog spec = specProperty True spec prog
  neverFulfills prog spec = specProperty False spec prog
  fulfillsNotFor ins prog spec =
    let trace = runProgram ins prog
    in property . not $ spec `accept` trace

instance (Show b, Arbitrary b, IOTestable a' b', Coercible b a) => IOTestable (a -> a') (b -> b') where
  fulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `fulfills` g x)
  neverFulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `neverFulfills` g x)
  fulfillsNotFor ins f g = forAllShrink arbitrary shrink (\x -> fulfillsNotFor ins (f (coerce x)) (g x))

instance CleverIOTestable (IOrep ()) (Specification (PTerm Varname)) where
  fulfillsClever prog spec =
    let
      prop input = case prog `matchesTrace` fromJust (traceForSequence input spec) of
        (MatchSuccessfull,(len, trace)) -> addCounterexample MatchSuccessfull (len,trace) True
        (err, (len, trace)) -> addCounterexample err (len,trace) False
    in forAllCleverInputs @Property spec prop

-- target represents the expected outcome of checking the property,
-- i.e. the property is satisfied iff for all inputs the performed check returns target
specProperty :: (SemTerm t (Environment Varname), VarListTerm t Varname) => Bool -> Specification t -> IOrep () -> Property
specProperty target spec program =
  let gen = traceGen spec
      prop tg = case program `matchesTrace` tg of
          (MatchSuccessfull,(len, trace)) -> addCounterexample MatchSuccessfull (len,trace) target
          (err, (len, trace)) -> addCounterexample err (len,trace) $ not target
  in forAllShow gen (PP.render . printGenNTraceInfo) prop

matchesTrace :: IOrep () -> GeneralizedTrace -> (MatchResult, (Int, OrdinaryTrace))
matchesTrace program tg =
  let inputs = inputsN tg
      len = length inputs
      trace = runProgram inputs program
      result = trace `isCoveredBy` tg
  in (result,(len,trace))

addCounterexample :: Testable prop => MatchResult -> (Int,OrdinaryTrace) -> prop -> Property
addCounterexample res (n,trace) = counterexample . PP.render $
  PP.hang (PP.text "Actual run:") 4
    (ppTrace n trace)
  PP.$$ PP.hang (PP.text "Error:") 4 errorMsg
  where errorMsg = case res of
          MatchSuccessfull -> PP.text "Expected error, but matching succeeded"
          err -> ppResult err

accept :: (SemTerm t (Environment Varname), VarListTerm t Varname) => Specification t -> OrdinaryTrace -> Bool
accept s@(Spec as) t = accept' as kI t (freshEnvironment (specVars s)) where
  kI Exit _    _ = error "loop exit marker on toplevel"
  kI End  Stop _ = True
  kI End  _    _ = False

accept' :: SemTerm t (Environment Varname) => [Action (Specification t) t] -> (Cont -> OrdinaryTrace -> Environment Varname -> Bool) -> OrdinaryTrace -> Environment Varname -> Bool
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
