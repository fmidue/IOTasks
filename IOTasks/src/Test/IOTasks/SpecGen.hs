{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- module Test.IOTasks.SpecGen (specGen, loopBodyGen) where
module Test.IOTasks.SpecGen where

import Data.Functor.Identity

import Data.Environment (Varname)
import Test.IOTasks.ValueSet (ValueSet)
import Test.IOTasks.Specification
import Test.IOTasks.Language (exit, ints,nats, getCurrent, getAll, var, StringEmbedding)
import Test.IOTasks (SpecTerm, branch)

import Data.Term (PVarTerm,VarListTerm)
import Data.Term.Liftable (Liftable, litT)
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
  ~(Spec [TillE s]) <- loop [("xs",ints)] (progressCondition [("xs",ints)] ["n"])
  return (s, Spec [ReadInput "n" nats])

-- generator for simple specifications
simpleSpec :: (Liftable t, PVarTerm t Varname) => Gen (Specification t)
simpleSpec = do
  i <- input [("n",nats)]
  l <- loop [("xs",ints),("y",ints)] (progressCondition [("xs",ints)] ["n"])
  p <- outputOneof (intTerm ["xs","y"] ["n"])
  return $ i <> l <> p

-- generator for pairs of very simple (no loops) similar specifications
-- TODO: fix terms often containing []
simpleSimilar :: (VarListTerm t Varname, Liftable t, PVarTerm t Varname) => Gen (Specification t, Specification t)
simpleSimilar = do
  (l1,vs) <- linearSpec lists xs
  (l2,_) <- linearSpec lists xs
  (l3,_) <- linearSpec lists xs
  (c1,c2) <- differentConditions (fst <$> lists) vs
  return
    ( l1 <> branch c1 l2 l3
    , l1 <> branch c2 l2 l3
    )
  where
    lists = [("x",ints),("y",ints)]
    xs = [("n", nats), ("m", ints)]

-- basic generators
input :: [(Varname,ValueSet)] -> Gen (Specification t)
input xs = do
  (x,vs) <- elements xs
  return $ Spec [ReadInput x vs]

someInputs :: [(Varname,ValueSet)] -> Int -> Gen (Specification t)
someInputs xs nMax = do
  n <- choose (1,nMax)
  is <- vectorOf n $ input xs
  return $ mconcat is

outputOneof :: (Typeable a, StringEmbedding a) => Gen (t a) -> Gen (Specification t)
outputOneof = outputOneof' False

optionalOutputOneof :: (Typeable a, StringEmbedding a) => Gen (t a) -> Gen (Specification t)
optionalOutputOneof = outputOneof' True

outputOneof' :: (Typeable a, StringEmbedding a) => Bool -> Gen (t a) -> Gen (Specification t)
outputOneof' b ts = do
  t <- ts
  return $ Spec [WriteOutput b [var 0] [t]]

outputSome :: (Typeable a, StringEmbedding a) => Gen (t a) -> Gen (Specification t)
outputSome = outputSome' False

optionalOutputSome :: (Typeable a, StringEmbedding a) => Gen (t a) -> Gen (Specification t)
optionalOutputSome = outputSome' True

outputSome' :: (Typeable a, StringEmbedding a) => Bool -> Gen (t a) -> Gen (Specification t)
outputSome' b ts = do
  ts' <- resize 5 $ listOf1 ts
  return $ Spec [WriteOutput b [var 0] ts']

-- generators with more complex invariants

-- branch free sequence of inputs and outputs
linearSpec :: (VarListTerm t Varname, PVarTerm t Varname, Liftable t) => [(Varname,ValueSet)] -> [(Varname,ValueSet)] -> Gen (Specification t, [Varname])
linearSpec lists xs = sized $ \n -> do
  is <- someInputs (lists ++ xs) (n `div` 2)
  let vs = specVars is
  os <- resize (n `div` 2) $ listOf1 $ outputOneof (intTerm vs vs)
  let spec = is <> mconcat os
  return (spec,specVars spec)

data Condition t = Condition { condTerm :: t Bool, progressInfo :: (Varname,ValueSet) }

-- generates a numeric condition of the form
-- f xs `comp` n
-- that contains every variable at most once
progressCondition :: (PVarTerm t Varname, Liftable t) => [(Varname,ValueSet)] -> [Varname] -> Gen (Condition t)
progressCondition lists nums = do
  comp <- elements [(T.>)]
  n <- oneof [getCurrent @Int <$> elements nums, litT <$> choose (0,10)]
  (xs,vs) <- elements lists
  f <- elements [T.length]
  return $ Condition (f (getAll @Int xs) `comp` n) (xs,vs)

-- simple loop
loop :: Liftable t => [(Varname,ValueSet)] -> Gen (Condition t) -> Gen (Specification t)
loop xs loopCondition = do
  Condition c (xp,vs) <- loopCondition
  let progress = ReadInput xp vs
  s1 <- someInputs xs 2
  s1' <- insert progress s1
  s <- oneof
    [ return $ Branch c s1' exit
    , return $ Branch (T.not c) exit s1'
    ]
  return $ Spec [TillE $ Spec [s]]

insert :: Action (Specification t) t -> Specification t -> Gen (Specification t)
insert a (Spec as) = do
  ix <- choose (0,length as)
  return . Spec $ take ix as ++ [a] ++ drop ix as

-- generators for terms and conditions

intTerm :: (PVarTerm t Varname, Liftable t) => [Varname] -> [Varname] -> Gen (t Int)
intTerm lists xs =
  oneof $ concat
    [ [ unary  | not $ null lists ]
    , [ var    | not $ null xs]
    , [ binary | not $ null xs]
    ]
  where
    var = do
      x <- elements xs
      return $ getCurrent x
    unary = do
      f <- elements [T.sum, T.length]
      x <- elements lists
      return $ f $ getAll x
    binary = do
      f <- elements [(T.+), (T.-), (T.*)]
      x <- elements xs
      y <- elements xs
      return $ f (getCurrent x) (getCurrent y)

condition :: (PVarTerm t Varname, Liftable t) => [Varname] -> [Varname] -> Gen (t Bool)
condition lists xs = runIdentity <$> condition' (fmap Identity . elements) lists xs

newtype P a = P { unP :: (a,a) } deriving Functor

differentConditions :: (PVarTerm t Varname, Liftable t) => [Varname] -> [Varname] -> Gen (t Bool, t Bool)
differentConditions lists xs = unP <$> condition' (fmap P . choosePair) lists xs

condition' :: (PVarTerm t Varname, Liftable t, Functor f) => (forall a. [a] -> Gen (f a)) -> [Varname] -> [Varname] -> Gen (f (t Bool))
condition' select lists xs = oneof [unary, binary]
  where
    unary = do
      x <- elements lists
      ff <- select [T.null, T.not . T.null]
      return $ (\f -> f $ getAll @Int x) <$> ff
    binary = do
      fop <- select [(T.==),(T.>),(T.<)]
      t1 <- intTerm lists xs
      t2 <- intTerm lists xs
      return $ (\op -> t1 `op` t2) <$> fop

-- choose two different values from a list
choosePair :: [a] -> Gen (a,a)
choosePair xs = elements [ (a,b) | (a,i) <- options, (b,j) <- options, i /= j ]
  where options = zip xs ([1..] :: [Int])

-- currently not used
-- splits (non-negative) n into k (non-negative) values s.t. sum [n_1,..,n_k] == n
splitSizeIn :: Int -> Int -> Gen [Int]
splitSizeIn k n = elements $ go k n where
  go :: Int -> Int -> [[Int]]
  go 1 n = [[n]]
  go j m = concat [ (i:) <$> go (j-1) (m-i) | i <- [0..m] ]
