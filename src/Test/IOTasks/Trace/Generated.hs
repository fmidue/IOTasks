{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Trace.Generated
  -- * Traces with randomly instantiated variables
  ( AbstractTrace
  , abstractTrace
  , SomeVarSet, (<|)
  , generateTrace
  -- * Traces with SMT-solvable constraints
  , ConstrainedTrace
  , constrainedTrace
  , var
  , solveTrace
  ) where

import Control.Monad ( foldM )

import Data.Some

import Test.QuickCheck
import Type.Reflection
import Type.Match

import Test.IOTasks.Term
import Test.IOTasks.Term.Prelude
import Test.IOTasks.Constraints
import Test.IOTasks.Trace.NormalizedTrace
import Test.IOTasks.ValueMap
import Test.IOTasks.ValueSet
import Test.IOTasks.Var
import Test.IOTasks.Z3

newtype AbstractTrace = AbstractTrace (Size -> Gen NTraceGeneralized)

generateTrace :: Size -> AbstractTrace -> Gen NTraceGeneralized
generateTrace sz (AbstractTrace f) = f sz

data SomeVarSet where
  SomeVarSet :: Typeable a => Var a -> ValueSet a -> SomeVarSet

(<|) :: Typeable a => Var a -> ValueSet a -> SomeVarSet
(<|) = SomeVarSet

-- |
-- Example usage:
--
-- @
-- abstractTrace [x \<| ints, y \<| ints] $ \\value ->
--   progReadLine (show $ value x) $
--   progReadLine (show $ value y) $
--   genProgWriteLine (Mandatory, Set.singleton (text $ show $ value x + value y))
--   terminate
-- where
--   x = intVar "x"
--   y = intVar "y"
-- @
abstractTrace :: [SomeVarSet] -> ((forall a. Typeable a => Var a -> a) -> NTraceGeneralized) -> AbstractTrace
abstractTrace vs f = AbstractTrace $ \size -> do
  m <- genValueMap size vs
  pure $ f (valueFunction m)

valueFunction :: Typeable a => ValueMap -> Var a -> a
valueFunction m (x :: Var a) = matchType @a
  [ inCaseOfE' @Integer $ \HRefl -> extractSingleValue $ lookupInteger (someVar x) m
  , inCaseOfE' @String $ \HRefl -> extractSingleValue $ lookupString (someVar x) m
  , inCaseOfE' @Bool $ \HRefl -> extractSingleValue $ lookupBool (someVar x) m
  , fallbackCase' $ error "should not happen"
  ]
  where
    extractSingleValue :: Maybe [(a,Int)] -> a
    extractSingleValue (Just [(v,_)]) = v
    extractSingleValue _ = error "invariant violated"

genValueMap :: Size -> [SomeVarSet] -> Gen ValueMap
genValueMap size = foldM addNextValue (emptyValueMap [])
  where
    addNextValue :: ValueMap -> SomeVarSet -> Gen ValueMap
    addNextValue m (SomeVarSet x vs) = do
      v <- valueOf x m vs size
      pure $ insertValue (wrapValue v) (someVar x) m

newtype ConstrainedTrace = ConstrainedTrace (Size -> Timeout -> IO NTraceGeneralized)

solveTrace :: Size -> Timeout -> ConstrainedTrace -> IO NTraceGeneralized
solveTrace sz to (ConstrainedTrace f) = f sz to

-- |
-- Usage example:
--
-- @
-- constrainedTrace [ x \<| ints, y \<| ints, z \<| ints ]
--   [ var x .+. var y ./=. intLit 0
--   , var y .+. var z .==. intLit 0
--   ]
--   $ \\value ->
--     progReadLine (show $ value x) $
--     progReadLine (show $ value y) $
--     progReadLine (show $ value z) $
--     genProgWriteLine (Mandatory, Set.singleton $ wildcard <> text "3" <> wildcard)
--     terminate
--   where
--     x = intVar "x"
--     y = intVar "y"
--     z = intVar "z"
-- @
constrainedTrace :: [SomeVarSet] -> [Term 'Transparent Bool] -> ((forall a. Typeable a => Var a -> a) -> NTraceGeneralized) -> ConstrainedTrace
constrainedTrace xs cs f = ConstrainedTrace $ \(Size bnd len) timeout -> do
  let
    inputs = map (\(SomeVarSet x vs) -> Some $ SimpleInput KeepInput x vs) xs
    conditions = map (Some . SimpleCondition) cs
    path = completePath [] $ ClosedPath $ inputs ++ conditions
  res <- findPathInput timeout path bnd len False
  case res of
    SAT is -> pure $ f (valueFunction $ mkValueMap $ zip (map (\(SomeVarSet x _) -> someVar x) xs) is)
    NotSAT -> error "trace not solvable"
    Timeout -> error "timeout"

-- | alias for 'currentValue' with a name more inline with the usage when constructing 'ConstrainedTrace' values
var :: Typeable a => Var a -> Term 'Transparent a
var = currentValue

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
