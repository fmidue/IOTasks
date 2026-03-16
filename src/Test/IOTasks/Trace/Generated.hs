{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}
module Test.IOTasks.Trace.Generated
  ( AbstractTrace
  , AbstractTrace'
  , SomeVarSet, (<|)
  , progReadAbstract
  , withValueConstraints
  , value
  , generateTrace
  , solveTrace
  ) where

import Control.Monad ( foldM )

import Data.Some
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck
import Type.Reflection
import Type.Match

import Test.IOTasks.Term
import Test.IOTasks.Term.Prelude
import Test.IOTasks.Constraints
import Test.IOTasks.Trace.NormalizedTrace
import Test.IOTasks.ValueMap as ValueMap
import Test.IOTasks.ValueSet
import Test.IOTasks.Var
import Test.IOTasks.Z3
import Test.IOTasks.Trace (Trace (terminate, progReadLine, progWriteLine, progReadChar), GeneralizedTrace (genProgWriteLine, genProgWrite))
import Test.IOTasks.Internal.OutputPattern (OutputPattern, PatternKind (..), text, evalPatternSet)

type AbstractTrace = AbstractTrace' (OptFlag, Set (OutputPattern SpecificationP))
data AbstractTrace' a where
  AbstractRead :: SomeVarSet -> AbstractTrace' a -> AbstractTrace' a
  ConcreteRead :: String -> AbstractTrace' a -> AbstractTrace' a
  ValueConstraint :: Term 'Transparent Bool -> AbstractTrace' a -> AbstractTrace' a
  WriteAT :: a -> AbstractTrace' a -> AbstractTrace' a
  WriteLineAT :: a -> AbstractTrace' a -> AbstractTrace' a
  TerminateAT :: AbstractTrace' a

data SomeVarSet where
  SomeVarSet :: (Typeable a, Show a) => Var a -> ValueSet a -> SomeVarSet

(<|) :: (Typeable a, Show a) => Var a -> ValueSet a -> SomeVarSet
(<|) = SomeVarSet

-- | alias for 'currentValue' with a name more inline with the usage when constructing 'AbstractTrace' and 'ConstrainedTrace' values
value :: Typeable a => Var a -> Term 'Transparent a
value = currentValue

progReadAbstract :: SomeVarSet -> AbstractTrace' a -> AbstractTrace' a
progReadAbstract = AbstractRead

instance Semigroup (AbstractTrace' a) where
  TerminateAT <> t = t
  t <> TerminateAT = t
  AbstractRead i t <> t' = AbstractRead i (t <> t')
  ConcreteRead i t <> t' = ConcreteRead i (t <> t')
  ValueConstraint c t <> t' = ValueConstraint c (t <> t')
  WriteAT o t <> t' = WriteAT o (t <> t')
  WriteLineAT o t <> t' = WriteLineAT o (t <> t')

instance Monoid (AbstractTrace' a) where
  mempty = TerminateAT

instance Trace (AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP))) where
  progReadChar = ConcreteRead . pure
  progReadLine = ConcreteRead
  progWriteLine = WriteAT . (Mandatory,) . Set.singleton . text
  terminate = TerminateAT

instance GeneralizedTrace AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP)) where
  genProgWrite = WriteAT
  genProgWriteLine = WriteLineAT

-- | random search for variable assignments
--   (generation will loop forever on unsatisfiable constraints)
generateTrace :: Size -> AbstractTrace -> Gen NTraceGeneralized
generateTrace sz t = genTrace `orErrorFrom` checkInputVariables "generateTrace" t
  where
    constraints = traceValueConstraints t
    genTrace = do
      m <- genValueMap sz (traceInputVariables t)
      if all (snd . oEval m) constraints
        then pure $ instantiateTrace m t
        else genTrace

instantiateTrace :: ValueMap -> AbstractTrace -> NTraceGeneralized
instantiateTrace m (ConcreteRead x t) = progReadLine x $ instantiateTrace m t
instantiateTrace m (AbstractRead (SomeVarSet x vs) t) =
  case oEval m (value x) of
    (_,v)
      | not (containsValue x m vs v) -> error $ concat ["value for ", show x, " is not in ", showValueSet vs]
      | otherwise -> progReadLine (show v) $ instantiateTrace m t
instantiateTrace m (WriteAT (o,x) t) =
  let (_,x') = evalPatternSet m x
  in genProgWrite (o,x') $ instantiateTrace m t
instantiateTrace m (WriteLineAT (o,x) t) =
  let (_,x') = evalPatternSet m x
  in genProgWriteLine (o,x') $ instantiateTrace m t
instantiateTrace m (ValueConstraint c t) =
  case oEval m c of
    (_,True) -> instantiateTrace m t
    (_,False) -> error $ "valueMap does not satisfy condition " ++ showTerm c
instantiateTrace _ TerminateAT = terminate

checkInputVariables :: String -> AbstractTrace' a -> Maybe String
checkInputVariables fn = reportDuplicates . Map.filter (>1) . countVariables
  where
    reportDuplicates :: Map SomeVar Int -> Maybe String
    reportDuplicates m
      | Map.null m = Nothing
      | otherwise = Just $ concat [fn,": duplicate variables: ", show $ Map.keys m]

    countVariables :: AbstractTrace' a -> Map SomeVar Int
    countVariables =
        Map.fromListWith (+)
          . map (\(SomeVarSet x _) -> (someVar x,1))
          . traceInputVariables

orErrorFrom :: a -> Maybe String -> a
orErrorFrom = (`maybe` error )

traceInputVariables :: AbstractTrace' a -> [SomeVarSet]
traceInputVariables (ConcreteRead _ t) = traceInputVariables t
traceInputVariables (AbstractRead vs t) = vs : traceInputVariables t
traceInputVariables (WriteAT _ t) = traceInputVariables t
traceInputVariables (ValueConstraint _ t) = traceInputVariables t
traceInputVariables (WriteLineAT _ t) = traceInputVariables t
traceInputVariables TerminateAT = []

traceValueConstraints :: AbstractTrace' a -> [Term 'Transparent Bool]
traceValueConstraints (ConcreteRead _ t) = traceValueConstraints t
traceValueConstraints (AbstractRead _ t) = traceValueConstraints t
traceValueConstraints (WriteAT _ t) = traceValueConstraints t
traceValueConstraints (ValueConstraint c t) = c : traceValueConstraints t
traceValueConstraints (WriteLineAT _ t) = traceValueConstraints t
traceValueConstraints TerminateAT = []

genValueMap :: Size -> [SomeVarSet] -> Gen ValueMap
genValueMap size = foldM addNextValue (emptyValueMap [])
  where
    addNextValue :: ValueMap -> SomeVarSet -> Gen ValueMap
    addNextValue m (SomeVarSet x vs) = do
      v <- valueOf x m vs size
      pure $ insertValue (wrapValue v) (someVar x) m

withValueConstraints :: AbstractTrace' a -> [Term 'Transparent Bool] -> AbstractTrace' a
withValueConstraints t cs = t <> foldr valueConstraint t cs

valueConstraint :: Term 'Transparent Bool -> AbstractTrace' a -> AbstractTrace' a
valueConstraint = ValueConstraint

-- | search for variable assignments using an SMT solver
solveTrace :: Size -> Timeout -> AbstractTrace -> IO NTraceGeneralized
solveTrace (Size bnd len) to t = do
  let
    xs = traceInputVariables t
    inputs = map (\(SomeVarSet x vs) -> Some $ SimpleInput KeepInput x vs) xs
    constraints = traceValueConstraints t
    conditions = map (Some . SimpleCondition) constraints
    path = completePath [] $ ClosedPath $ inputs ++ conditions
  res <- findPathInput to path bnd len False
  case res of
    SAT is -> pure $ instantiateTrace (mkValueMap $ zip (map (\(SomeVarSet x _) -> someVar x) xs) is) t
    NotSAT -> error "trace not solvable"
    Timeout -> error "timeout"

mkValueMap :: [(SomeVar,String)] -> ValueMap
mkValueMap = foldl addNextValue (emptyValueMap [someVar $ intVar "x1"])
  where
    addNextValue :: ValueMap -> (SomeVar,String) -> ValueMap
    addNextValue m (sx@(SomeVar (_ :: Var a)), s) = matchType @a
      [ inCaseOf' @Integer $ insertValue (wrapValue (read @Integer s)) sx m
      , inCaseOf' @String $ insertValue (wrapValue s) sx m
      , inCaseOf' @Bool $ insertValue (wrapValue (read @Bool s)) sx m
      , fallbackCase' $ error "should not happen"
      ]
