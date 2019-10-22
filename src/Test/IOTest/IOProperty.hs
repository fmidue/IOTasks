{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.IOProperty (
  fulfills,
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

import Debug.Trace

class IOTestable a b where
  fulfills :: a -> b -> Property
  fulfillsNotFor :: [String] -> a -> b -> Property

instance IOTestable (IOrep ()) Specification where
  fulfills prog spec = specProperty spec prog
  fulfillsNotFor ins prog spec =
    let trace = runProgram ins prog
    in property . not $ spec `accept` trace

instance (Show b, Arbitrary b, IOTestable a' b', Coercible b a) => IOTestable (a -> a') (b -> b') where
  fulfills f g = forAllShrink arbitrary shrink (\x -> f (coerce x) `fulfills` g x)
  fulfillsNotFor ins f g = forAllShrink arbitrary shrink (\x -> fulfillsNotFor ins (f (coerce x)) (g x))

specProperty :: Specification -> IOrep () -> Property
specProperty spec program =
  let gen = normalizeG <$> traceGen spec
      prop t = testTrace ((id &&& inputsG) t) program
  in forAllShow gen (render . printGenNTraceInfo) prop

testTrace :: (GeneralizedNTrace, [String]) -> IOrep () -> Property
testTrace (tg,ins) p =
  let trace = runProgram ins p
      normalized = normalizeO trace
  in case normalized `isCoveredBy` tg of
      MatchSuccessfull -> property True
      err -> counterexample (render $
        hang (text "Actual run:") 4
          (pPrint trace)
       $$ hang (text "Error:") 4
         (ppResult err) )
       False

  --in _ -- counterexample (msg ++ "\n  program trace: " ++ show normalized) result

accept :: Specification -> OrdinaryTrace -> Bool
accept s@(Spec as) t = accept' as kI t (freshEnvironment s) where
  kI Exit _         _ = error "loop exit marker on toplevel"
  kI End  (OT Stop) _ = True
  kI End  (OT _   ) _ = False

accept' :: [Action] -> (Cont -> OrdinaryTrace -> Environment -> Bool) -> OrdinaryTrace -> Environment -> Bool
accept' (ReadInput x ty : s') k (OT (ProgRead v t')) env =
  let val = valueFromString ty v
      env' = fromMaybe (error "accept: environment update failed") (updateWithValue x val env)
  in containsValue ty val && accept' s' k (OT t') env'
accept' (ReadInput x ty : s') k t env = False
accept' (WriteOutput True ps ts : s') k t env =
  accept' (WriteOutput False ps ts : s') k t env || accept' s' k t env
accept' (WriteOutput False ps ts : s') k (OT (ProgWrite v t')) env =
  let vs = traceShowId $ (\p -> fillHoles (p,ts) env) <$> ps
  in any (v `isContainedIn`) vs && accept' s' k (OT t') env
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
