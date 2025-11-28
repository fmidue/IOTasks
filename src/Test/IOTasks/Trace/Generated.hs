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
  -- * Traces with randomly instantiated variables
  ( AbstractTrace
  , AbstractTrace'
  , SomeVarSet, (<|)
  , AbstractRead(progReadAbstract)
  , generateTrace
  , var
  -- * Traces with SMT-solvable constraints
  , ConstrainedTrace
  , ConstrainedTrace'
  , valueConstraint
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

type AbstractTrace = AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP))
newtype AbstractTrace' a = AbstractTrace { abstractStructure :: AbstractTraceStructure 'NoConstraints a}
  deriving (Semigroup,Monoid) via (AbstractTraceStructure 'NoConstraints a)

data AbstractTraceStructure (k :: TraceFlavor) a where
  AbstractRead :: SomeVarSet -> AbstractTraceStructure k a -> AbstractTraceStructure k a
  ConcreteRead :: String -> AbstractTraceStructure k a -> AbstractTraceStructure k a
  ValueConstraint :: Term 'Transparent Bool -> AbstractTraceStructure 'WithConstraints a -> AbstractTraceStructure 'WithConstraints a
  WriteAT :: a -> AbstractTraceStructure k a -> AbstractTraceStructure k a
  WriteLineAT :: a -> AbstractTraceStructure k a -> AbstractTraceStructure k a
  TerminateAT :: AbstractTraceStructure k a

data TraceFlavor = NoConstraints | WithConstraints

data SomeVarSet where
  SomeVarSet :: (Typeable a, Show a) => Var a -> ValueSet a -> SomeVarSet

(<|) :: (Typeable a, Show a) => Var a -> ValueSet a -> SomeVarSet
(<|) = SomeVarSet

-- | alias for 'currentValue' with a name more inline with the usage when constructing 'AbstractTrace' and 'ConstrainedTrace' values
var :: Typeable a => Var a -> Term 'Transparent a
var = currentValue

liftAbstract :: (AbstractTraceStructure 'NoConstraints a -> AbstractTraceStructure 'NoConstraints a) -> AbstractTrace' a -> AbstractTrace' a
liftAbstract f = AbstractTrace . f . abstractStructure

class Trace t => AbstractRead t where
  progReadAbstract :: SomeVarSet -> t -> t

instance AbstractRead (AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP))) where
  progReadAbstract = liftAbstract . AbstractRead

instance Semigroup (AbstractTraceStructure k a) where
  TerminateAT <> t = t
  t <> TerminateAT = t
  AbstractRead i t <> t' = AbstractRead i (t <> t')
  ConcreteRead i t <> t' = ConcreteRead i (t <> t')
  ValueConstraint c t <> t' = ValueConstraint c (t <> t')
  WriteAT o t <> t' = WriteAT o (t <> t')
  WriteLineAT o t <> t' = WriteLineAT o (t <> t')

instance Monoid (AbstractTraceStructure k a) where
  mempty = TerminateAT

instance Trace (AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP))) where
  progReadChar = liftAbstract . ConcreteRead . pure
  progReadLine = liftAbstract . ConcreteRead
  progWriteLine = liftAbstract . WriteAT . (Mandatory,) . Set.singleton . text
  terminate = AbstractTrace TerminateAT

instance GeneralizedTrace AbstractTrace' (OptFlag, Set (OutputPattern 'SpecificationP)) where
  genProgWrite = liftAbstract . WriteAT
  genProgWriteLine = liftAbstract . WriteLineAT

generateTrace :: Size -> AbstractTrace -> Gen NTraceGeneralized
generateTrace sz (AbstractTrace t) = genTrace `orErrorFrom` checkInputVariables "generateTrace" t
  where
    genTrace = do
      m <- genValueMap sz (traceInputVariables t)
      pure $ instantiateTrace m t

instantiateTrace :: ValueMap -> AbstractTraceStructure k (OptFlag, Set (OutputPattern 'SpecificationP)) -> NTraceGeneralized
instantiateTrace m (ConcreteRead x t) = progReadLine x $ instantiateTrace m t
instantiateTrace m (AbstractRead (SomeVarSet x vs) t) =
  case oEval m (var x) of
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

checkInputVariables :: String -> AbstractTraceStructure k a -> Maybe String
checkInputVariables fn = reportDuplicates . Map.filter (>1) . countVariables
  where
    reportDuplicates :: Map SomeVar Int -> Maybe String
    reportDuplicates m
      | Map.null m = Nothing
      | otherwise = Just $ concat [fn,": duplicate variables: ", show $ Map.keys m]

    countVariables :: AbstractTraceStructure k a -> Map SomeVar Int
    countVariables =
        Map.fromListWith (+)
          . map (\(SomeVarSet x _) -> (someVar x,1))
          . traceInputVariables

orErrorFrom :: a -> Maybe String -> a
orErrorFrom = (`maybe` error )

traceInputVariables :: AbstractTraceStructure k a -> [SomeVarSet]
traceInputVariables (ConcreteRead _ t) = traceInputVariables t
traceInputVariables (AbstractRead vs t) = vs : traceInputVariables t
traceInputVariables (WriteAT _ t) = traceInputVariables t
traceInputVariables (ValueConstraint _ t) = traceInputVariables t
traceInputVariables (WriteLineAT _ t) = traceInputVariables t
traceInputVariables TerminateAT = []

traceValueConstraints :: AbstractTraceStructure k a -> [Term 'Transparent Bool]
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

type ConstrainedTrace = ConstrainedTrace' (OptFlag, Set (OutputPattern 'SpecificationP))
newtype ConstrainedTrace' a = ConstrainedTrace { constrainedStructure :: AbstractTraceStructure 'WithConstraints a}
    deriving (Semigroup,Monoid) via (AbstractTraceStructure 'WithConstraints a)

liftConstrained :: (AbstractTraceStructure 'WithConstraints a -> AbstractTraceStructure 'WithConstraints a) -> ConstrainedTrace' a -> ConstrainedTrace' a
liftConstrained f = ConstrainedTrace . f . constrainedStructure

valueConstraint :: Term 'Transparent Bool -> ConstrainedTrace -> ConstrainedTrace
valueConstraint = liftConstrained . ValueConstraint

instance AbstractRead (ConstrainedTrace' (OptFlag, Set (OutputPattern 'SpecificationP))) where
  progReadAbstract = liftConstrained . AbstractRead

instance Trace (ConstrainedTrace' (OptFlag, Set (OutputPattern 'SpecificationP))) where
  progReadChar = liftConstrained . ConcreteRead . pure
  progReadLine = liftConstrained . ConcreteRead
  progWriteLine = liftConstrained . WriteAT . (Mandatory,) . Set.singleton . text
  terminate = ConstrainedTrace TerminateAT

instance GeneralizedTrace ConstrainedTrace' (OptFlag, Set (OutputPattern 'SpecificationP)) where
  genProgWrite = liftConstrained . WriteAT
  genProgWriteLine = liftConstrained . WriteLineAT

solveTrace :: Size -> Timeout -> ConstrainedTrace -> IO NTraceGeneralized
solveTrace (Size bnd len) to (ConstrainedTrace t) = do
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
