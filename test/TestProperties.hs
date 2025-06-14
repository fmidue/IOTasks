{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestProperties (testCheapProperties, testExpensiveProperties) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (isSuccess, stdArgs, Args)

import Control.Monad.Loops (allM)

import Test.IOTasks
import Test.IOTasks.Specification (LoopBody(..))
import Test.IOTasks.Random (genInput)
import qualified Test.IOTasks.Random as Random (stdArgs, Args(..))
import Test.IOTasks.ValueSet
import Test.IOTasks.Trace (ordinaryTrace, isTerminatingN, normalizedTrace)
import Test.IOTasks.OutputPattern (PatternKind(TraceP),(>:), nonEmptyWildcard)

testExpensiveProperties :: Spec
testExpensiveProperties = do
  context "testing with random specifications" $ do
    prop "programs built from a spec don't go wrong on (solver based) inputs generated from the same spec" $
      \s -> ioProperty $ do
        is <- generateStaticTestSuite stdArgs s
        pure $ all (\p -> all (isTerminatingN . normalizedTrace . runProgram p) is) (interpret s)

    prop "inputs are never optional for a fixed input prefix (traces obtained from too less inputs never terminate)" $
      \s -> ioProperty $ do
        is <- filter (not . null) <$> generateStaticTestSuite stdArgs s
        pure $ not (any (isTerminatingN . normalizedTrace . fst . runSpecification s) (init <$> is))

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
  context "testing with random specifications" $ do
    prop "programs built from a spec satisfy that spec" $
      \s -> allM (\p -> isSuccess <$> taskCheckWithOutcome stdArgs{terminalOutput=False,maxIterationUnfold=15,testsPerPath=5} p s) (interpret s) `shouldReturn` True

    prop "relate runSpecification based semantics to accept (cf FLOPS 2020)" $
      \s -> testAgainst s s

    prop "programs built from a spec don't go wrong on (random) inputs generated from the same spec" $
      \s ->
        let Random.Args{..} = Random.stdArgs
        in
          forAll (genInput s maxInputLength (Size inputRange (fromIntegral $ inputRange `div` 5)) maxNegative)
            (\is -> all (isTerminatingN . normalizedTrace . flip runProgram is) (interpret s))

  context "string pattern matching" $ do
    prop "wildcard >: x == True" $
      forAll genPattern $ \p -> wildcard >: p
    prop "reflexivity of >:" $
      forAll genPattern $ \p -> p >: p
    prop "transitivity of >:" $ --improve?
       --improve?
      forAll ((,,) <$> genPattern <*> genPattern <*> genPattern) $ \(x,y,z) -> not (not (x >: z) && (x >: y) && (y >: z))
    prop "antisymmetry of >:" $
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
        notEmpty <- not <$> isEmpty (intVar undefined) undefined vs
        pure $ notEmpty ==> forAll (valueOf @Integer (intVar undefined) undefined vs (Size 100 undefined)) $ initiallyContainsValue vs

    prop "a value is either in a ValueSet or in it's complement" $
      \vs -> forAll (vectorOf 100 $ arbitrary @Integer) $ \xs -> all (\x -> (vs `initiallyContainsValue` x) /= (complement vs `initiallyContainsValue` x)) xs

    prop "with adds an element to a ValueSet" $
      \vs x -> (vs `with` x) `initiallyContainsValue` x

    prop "without removes an element from a ValueSet" $
      \vs x -> not $ (vs `without` x) `initiallyContainsValue` x

    prop "vs \\ ws removes all elements of ws from vs" $
      \vs ws -> ioProperty $ do
        notEmpty <- not <$> isEmpty (intVar undefined) undefined ws
        pure $ notEmpty ==> forAll (valueOf @Integer (intVar undefined) undefined ws (Size 100 undefined)) $ not . initiallyContainsValue (vs \\ ws)

testEquiv :: Specification -> Specification -> Property
testEquiv s1 s2 = p1 Test.QuickCheck..&&. p2 where
  p1 = s1 `testAgainst` s2
  p2 = s2 `testAgainst` s1

testAgainst :: Specification -> Specification -> Property
testAgainst x y = ioProperty $ do
  r <- generateStaticTestSuite stdArgs y
  pure $ all (\i -> y `accept` ordinaryTrace (fst (runSpecification' False x i))) r

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
