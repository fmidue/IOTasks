{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- module Test.IOTasks.SpecGen (specGen, loopBodyGen) where
module Test.IOTasks.SpecGen where

import Data.Environment (Varname)
import Test.IOTasks.ValueSet (ValueSet)
import Test.IOTasks.Specification
import Test.IOTasks.Language (exit, ints,nats, getCurrent, getAll, var, StringEmbedding)
import Test.IOTasks (SpecTerm)

import Data.Term (PVarTerm)
import Data.Term.Liftable (Liftable)
import qualified Data.Term.Liftable.Prelude as T

import Test.QuickCheck

import Type.Reflection (Typeable)

-- limited versions of old generators for testing
-- TODO: improve for better coverage
specGen :: Gen (Specification SpecTerm)
specGen = simpleSpec

-- generates a loop body and in initialisation for the variables used as current values
loopBodyGen :: Gen (Specification SpecTerm,Specification SpecTerm)
loopBodyGen = do
  ~(Spec [TillE s]) <- loop [("xs",ints)] (numericCondition [("xs",ints)] ["n"])
  return (s, Spec [ReadInput "n" nats])

-- generator for simple specifications
simpleSpec :: (Liftable t, PVarTerm t Varname) => Gen (Specification t)
simpleSpec = do
  i <- input [("n",nats)]
  l <- loop [("xs",ints)] (numericCondition [("xs",ints)] ["n"])
  p <- outputOneof [T.sum $ getAll "xs", T.length @_ @_ @Int (getAll "xs")]
  return $ i <> l <> p

-- basic generators
input :: [(Varname,ValueSet)] -> Gen (Specification t)
input xs = do
  (x,vs) <- elements xs
  return $ Spec [ReadInput x vs]

outputOneof :: (Typeable a, StringEmbedding a) => [t a] -> Gen (Specification t)
outputOneof = outputOneof' False

optionalOutputOneof :: (Typeable a, StringEmbedding a) => [t a] -> Gen (Specification t)
optionalOutputOneof = outputOneof' True

outputOneof' :: (Typeable a, StringEmbedding a) => Bool -> [t a] -> Gen (Specification t)
outputOneof' b ts = do
  t <- elements ts
  return $ Spec [WriteOutput b [var 0] [t]]

outputSome :: (Typeable a, StringEmbedding a) => [t a] -> Gen (Specification t)
outputSome = outputSome' False

optionalOutputSome :: (Typeable a, StringEmbedding a) => [t a] -> Gen (Specification t)
optionalOutputSome = outputSome' True

outputSome' :: (Typeable a, StringEmbedding a) => Bool -> [t a] -> Gen (Specification t)
outputSome' b ts = do
  ts' <- sublistOf ts `suchThat` (not . null)
  return $ Spec [WriteOutput b [var 0] ts']

-- generators with more complex invariants
data Condition t = Condition { condTerm :: t Bool, progressInfo :: (Varname,ValueSet) }

-- generates a numeric condition of the form
-- f xs `comp` n
-- that contains every variable at most once
numericCondition :: (PVarTerm t Varname, Liftable t) => [(Varname,ValueSet)] -> [Varname] -> Gen (Condition t)
numericCondition lists nums = do
  comp <- elements [(T.>)]
  n <- elements nums
  (xs,vs) <- elements lists
  f <- elements [T.length]
  return $ Condition (f (getAll @Int xs) `comp` getCurrent @Int n) (xs,vs)

-- simple loop that just does progress
loop :: Liftable t => [(Varname,ValueSet)] -> Gen (Condition t) -> Gen (Specification t)
loop _xs loopCondition = do
  Condition c (xp,vs) <- loopCondition
  let progress = Spec [ReadInput xp vs]
  -- s1' <- insert progress s1
  s <- oneof
    [ return $ Branch c progress exit
    , return $ Branch (T.not c) exit progress
    ]
  return $ Spec [TillE $ Spec [s]]

insert :: Action (Specification SpecTerm) SpecTerm -> Specification SpecTerm -> Gen (Specification SpecTerm)
insert a (Spec as) = do
  ix <- choose (0,length as)
  return . Spec $ take ix as ++ [a] ++ drop ix as

-- currently not used
-- splits (non-negative) n into k (non-negative) values s.t. sum [n_1,..,n_k] == n
splitSizeIn :: Int -> Int -> Gen [Int]
splitSizeIn k n = elements $ go k n where
  go :: Int -> Int -> [[Int]]
  go 1 n = [[n]]
  go j m = concat [ (i:) <$> go (j-1) (m-i) | i <- [0..m] ]
