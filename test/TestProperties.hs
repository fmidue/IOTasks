{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
module TestProperties (testCheapProperties, testExpensiveProperties) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Test.QuickCheck hiding (isSuccess, stdArgs, Args)

import Control.Monad.Loops (allM)

import Test.IOTasks
import Test.IOTasks.Specification (isRelevantInput,LoopBody(..))
import Test.IOTasks.Random (genInput)
import qualified Test.IOTasks.Random as Random (stdArgs, Args(..))
import Test.IOTasks.ValueSet
import Test.IOTasks.Trace
import Test.IOTasks.OutputPattern (PatternKind(TraceP),(>:), nonEmptyWildcard)
import Test.IOTasks.Z3 (satPaths, isSatPath, SatResult (SAT))
import Test.IOTasks.Constraints (constraintTree, pathDepth, completePath, Path, Constraint (InputConstraint, ConditionConstraint))
import SumToZeroSpec (sumToZeroSpec)
import Data.Some (Some(Some))
import Type.Match (matchType, inCaseOfE', fallbackCase')
import Data.Typeable ((:~~:)(HRefl))
import Control.Monad (filterM)
import Test.IOTasks.ValueMap (emptyValueMap)
import Test.IOTasks.Var (someVar)

testExpensiveProperties :: Spec
testExpensiveProperties = do
  context "testing with random specifications" $ do
    prop "programs built from a spec don't go wrong on (solver based) inputs generated from the same spec" $
      \s -> ioProperty $ do
        is <- generateStaticTestSuite stdArgs s
        pure $ all (\p -> all (isTerminatingLineTrace . runProgram p) is) (interpret s)

    prop "inputs are never optional for a fixed input prefix (traces obtained from too less inputs never terminate)" $
      \s -> ioProperty $ do
        is <- filter (not . null) <$> generateStaticTestSuite stdArgs s
        pure $ not (any (isTerminatingNTrace . fst . runSpecification s) (init <$> is))

    prop "tillExit s === tillExit (s <> tillExit s <> exit) " $
      \(LoopBody s i) -> testEquiv
        (i <> tillExit s)
        (i <> tillExit (s <> tillExit s <> exit))

    prop "tillExit (s1 <> exit) === tillExit (s1 <> exit <> s2)" $
      \s1 s2 -> testEquiv
        (tillExit (s1 <> exit))
        (tillExit (s1 <> exit <> s2))

        -- extremely slow - TODO:improve
    xPerfprop "s1 <> (s2 <> s3) === (s1 <> s2) <> s3" $
      \s1 s2 s3 -> testEquiv
      (s1 <> (s2 <> s3))
      ((s1 <> s2) <> s3)

testCheapProperties :: Spec
testCheapProperties = do
  context "testing with fixed specifications" $ do
    prop "input generation can find all relevant inputs" $
      let s = sumToZeroSpec -- simple spec with no negative inputs, but non-trivial constraints
      in forAll
        (listOf (chooseInteger (-100,100))
          `suchThat`
          \is -> length is <= 25 &&isRelevantInput s (map show is))
        (\is -> ioProperty $ do
          paths <- map (completePath []) . fst <$> satPaths (0,25) 1000 (constraintTree s) 10 False
          (1 ==) . length
            <$> filterM (\p -> do
              res <- isSatPath 1000 p 10 False
              pure (length is == pathDepth p && res == SAT ())
            ) paths
        )

  context "testing with random specifications" $ do
    prop "programs built from a spec satisfy that spec" $
      \s -> allM (\p -> isSuccess <$> taskCheckWithOutcome stdArgs{terminalOutput=False,maxIterationUnfold=15,testsPerPath=5} p s) (interpret s) `shouldReturn` True

    prop "relate input generation and accept-based relevant inputs" $
      \s -> all (isRelevantInput s) <$> generateStaticTestSuite stdArgs s `shouldReturn` True

    prop "relate runSpecification based semantics to accept (cf FLOPS 2020)" $
      \s -> testAgainst s s

    prop "programs built from a spec don't go wrong on (random) inputs generated from the same spec" $
      \s ->
        let Random.Args{..} = Random.stdArgs
        in
          forAll (genInput s maxInputLength (Size inputRange (fromIntegral $ inputRange `div` 5)) maxNegative)
            (\is -> all (isTerminatingLineTrace . flip runProgram is) (interpret s))

  context "string pattern matching" $ do
    prop "wildcard >: x == True" $
      forAll genPattern $ \p -> wildcard >: p
    modifyMaxSuccess (*100) $ prop "reflexivity of >:" $
      forAll genPattern $ \p -> p >: p
    modifyMaxSuccess (*100) $ prop "transitivity of >:" $
      --improve?
      forAll ((,,) <$> genPattern <*> genPattern <*> genPattern) $ \(x,y,z) -> not (not (x >: z) && (x >: y) && (y >: z))
    modifyMaxSuccess (*100) $ prop "antisymmetry of >:" $
      forAll ((,) <$> genPattern <*> genPattern) $ \(x,y) -> (x==y) || not (x >: y && y >: x)

    prop "adding a wildcard results in a more general pattern" $
      forAll (liftA2 (,) genPattern genPattern) $
        \(p,q) -> (p <> wildcard <> q) >: (p <> q)

    prop "replacing a wildcard with a pattern yields a less general pattern" $
      forAll ((,,) <$> genPattern <*> genPattern <*> genPattern) $
        \(p,q,x) -> counterexample (unlines [show $ p <> wildcard <> q, show $ p <> x <> q]) $
          (p <> wildcard <> q) >: (p <> x <> q)

  context "ValueSet operations" $ do
    prop "no value in empty ValueSet" $
      \x -> not ((empty :: ValueSet Integer) `initiallyContainsValue` x)

    prop "every value in complete ValueSet" $
      \x -> (complete :: ValueSet Integer) `initiallyContainsValue` x

    prop "valueOf generates values from the set" $
      \vs -> ioProperty $ do
        let
          x = intVar "x"
          valueMap = emptyValueMap [someVar $ intVar "x"]
        notEmpty <- not <$> isEmpty x valueMap vs
        pure $ notEmpty ==> forAll (valueOf x valueMap vs (Size 100 undefined)) $ initiallyContainsValue vs

    prop "a value is either in a ValueSet or in it's complement" $
      \vs -> forAll (vectorOf 100 $ arbitrary @Integer) $ \xs -> all (\x -> (vs `initiallyContainsValue` x) /= (complement vs `initiallyContainsValue` x)) xs

    prop "with adds an element to a ValueSet" $
      \vs x -> (vs `with` x) `initiallyContainsValue` x

    prop "without removes an element from a ValueSet" $
      \vs x -> not $ (vs `without` x) `initiallyContainsValue` x

    prop "vs \\\\ ws removes all elements of ws from vs" $
      \vs ws -> ioProperty $ do
        let
          x = intVar "x"
          valueMap = emptyValueMap [someVar $ intVar "x"]
        notEmpty <- not <$> isEmpty x valueMap ws
        pure $ notEmpty ==> forAll (valueOf x valueMap ws (Size 100 undefined)) $ not . initiallyContainsValue (vs \\ ws)

  context "trace types" $ do
    describe "LineTrace" $ do
      generalTraceProperties @LineTrace

    describe "NTracePlain" $ do
      generalTraceProperties @NTracePlain

    describe "NTraceGeneralized" $ do
      generalTraceProperties @NTraceGeneralized

assertInputSequence :: [Integer] -> Path -> Path
assertInputSequence [] ((Some InputConstraint{}):_) = error "out of inputs"
assertInputSequence [] [] = []
assertInputSequence (_:_) [] = error "too many inputs"
assertInputSequence (i:is) (c@(Some (InputConstraint (var :: Var a,_) _ m)):p) =
  let
    new = matchType @a
      [inCaseOfE' @Integer $ \HRefl -> Some (ConditionConstraint (currentValue var .==. intLit i) m)
      , fallbackCase' $ error "only integers allowed"
      ]
  in c : new : assertInputSequence is p
assertInputSequence is (c:p) = c : assertInputSequence is p


generalTraceProperties :: forall t. (Trace t, Eq t) => Spec
generalTraceProperties = do
  prop "progReadLine adds a line break" $
    \(NonEmpty s) -> progReadLine @t s terminate == foldr progReadChar terminate (s <> "\n")

  prop "progReadLine can be split at linebreaks" $
    \(NonEmpty s) (NonEmpty t) -> progReadLine @t s (progReadLine t terminate) == progReadLine (s <> "\n" <> t) terminate

  prop "progWriteLine adds a line break" $
    \(NonEmpty s) -> let t = progWriteLine @t s terminate
      in t == progWriteStr (s <> "\n") terminate && t == foldr progWriteChar terminate (s <> "\n")

  prop "progWriteLine can be split at linebreaks" $
    \(NonEmpty s) (NonEmpty t) -> progWriteLine @t s (progWriteLine t terminate) == progWriteLine (s <> "\n" <> t) terminate


testEquiv :: Specification -> Specification -> Property
testEquiv s1 s2 = p1 Test.QuickCheck..&&. p2 where
  p1 = s1 `testAgainst` s2
  p2 = s2 `testAgainst` s1

testAgainst :: Specification -> Specification -> Property
testAgainst x y = ioProperty $ do
  r <- generateStaticTestSuite stdArgs y
  pure $ all (\i -> y `accept` (fst (runSpecification' False x i))) r

genPattern :: Gen (OutputPattern 'TraceP)
genPattern = sized $ \size ->
  frequency $
    [ (1,pure wildcard)
    , (1,pure nonEmptyWildcard)
    , (1,text <$> listOf1 (arbitraryPrintableChar `suchThat` (/= '_')))
    ] ++
    [ (4,liftA2 (<>) (resize 1 genPattern) (resize (size - 1) genPattern)) | size > 1
    ]

-- misc
xprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
xprop s = xit s . property

fprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
fprop s = fit s . property

-- disable a property for faster testing
xPerfprop :: HasCallStack => String -> prop -> Spec
xPerfprop s _ = it s $ pendingWith "disabled for faster testing"

-- TODO: add dependent value sets
instance Arbitrary (ValueSet Integer) where
  arbitrary = sized $ \size ->
    if size <= 1
      then oneof [pure complete, pure empty, singleton <$> arbitrary]
      else frequency
        [ (1,pure complete)
        , (1,pure empty)
        , (1,singleton <$> arbitrary)
        , (2,lessThan <$> resize (size-1) arbitrary)
        , (2,greaterThan <$> resize (size-1) arbitrary)
        , (2,intersection <$> (resize (size `div` 2) arbitrary) <*> resize (size `div` 2) arbitrary)
        , (2,union <$> (resize (size `div` 2) arbitrary) <*> resize (size `div` 2) arbitrary)
        ]

instance Show (ValueSet Integer) where
  show = showValueSet
