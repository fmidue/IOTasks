{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module TestProperties (testProperties) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (isSuccess, stdArgs,verbose, Args)

import Control.Monad.Loops (allM)

import Test.IOTasks
import Test.IOTasks.Random (genInput)
import qualified Test.IOTasks.Random as Random (stdArgs, Args(..))
import Test.IOTasks.ValueSet
import Test.IOTasks.Trace (ordinaryTrace, isTerminatingN, normalizedTrace)

import SpecificationGenerator

import Control.Applicative

testProperties :: Spec
testProperties = do
  context "testing with random specifications" $ do
    prop "programs built from a spec satisfy that spec" $
      forAll specGen $ \s -> allM (\p -> isSuccess <$> taskCheckWithOutcome stdArgs{verbose=False,maxIterationUnfold=15,maxSuccessPerPath=5} p s) (interpret s) `shouldReturn` True

    prop "programs built from a spec dont go wrong on (solver based) inputs generated from the same spec" $
      forAll specGen (\s -> ioProperty $ do
        is <- generateStaticTestSuite stdArgs s
        pure $ all (\p -> all (\i -> isTerminatingN $ runProgram i p) is) (interpret s)
      )

    prop "programs built from a spec dont go wrong on (random) inputs generated from the same spec" $
      forAll specGen (\s ->
        let Random.Args{..} = Random.stdArgs
        in forAll (genInput s maxPathDepth (Size valueSize (fromIntegral $ valueSize `div` 5)) maxNegative) (\is ->
          all (isTerminatingN . runProgram is) (interpret s)
      ))

    prop "relate runSpecification based semantics to accept (cf FLOPS 2020)" $
      forAll specGen (\s -> testAgainst s s)

    prop "inputs are never optional for a fixed input prefix (traces obtained from too less inputs never terminate)" $
      forAll specGen (\s -> ioProperty $ do
        is <- filter (not . null) <$> generateStaticTestSuite stdArgs s
        pure $ all (\i -> not . isTerminatingN . normalizedTrace . fst $ runSpecification i s) (init <$> is)
      )

    prop "tillExit s === tillExit (s <> tillExit s <> exit) " $
      forAll loopBodyGen $ \(s,i) -> testEquiv
        (i <> tillExit s)
        (i <> tillExit (s <> tillExit s <> exit))

    prop "sanity check" $
      \s -> s `testAgainst` s

    -- extremely slow - TODO:improve
    xPerfprop "s1 <> (s2 <> s3) === (s1 <> s2) <> s3" $
      \s1 s2 s3 -> testEquiv
          (s1 <> (s2 <> s3))
          ((s1 <> s2) <> s3)

    prop "tillExit (s1 <> exit) === tillExit (s1 <> exit <> s2)" $
      \s1 s2 -> testEquiv
        (tillExit (s1 <> exit))
        (tillExit (s1 <> exit <> s2))

  context "string pattern matching" $ do
    prop "wildcard >: x == True" $
      forAll genPattern $ \p -> Wildcard >: p
    prop "reflexivity of >:" $
      forAll genPattern $ \p -> p >: p
    prop "transitivity of >:" $ --improve?
      forAll ((,,) <$> genPattern <*> genPattern <*> genPattern) $ \(x,y,z) -> not (not (x >: z) && (x >: y) && (y >: z))
    prop "antisymmetry of >:" $
      forAll ((,) <$> genPattern <*> genPattern) $ \(x,y) -> (x==y) || not (x >: y && y >: x)

    prop "adding a wildcard results in a more general pattern" $
      forAll (liftA2 (,) genPattern genPattern) $
        \(p,q) -> (p <> Wildcard <> q) >: (p <> q)

    prop "replacing a wildcard with a pattern yields a less general pattern" $
      forAll ((,,) <$> genPattern <*> genPattern <*> genPattern) $
        \(p,q,x) -> counterexample (unlines [show $ p <> Wildcard <> q, show $ p <> x <> q]) $
          (p <> Wildcard <> q) >: (p <> x <> q)

testEquiv :: Specification -> Specification -> Property
testEquiv s1 s2 = p1 Test.QuickCheck..&&. p2 where
  p1 = s1 `testAgainst` s2
  p2 = s2 `testAgainst` s1

testAgainst :: Specification -> Specification -> Property
testAgainst x y = ioProperty $ do
  r <- generateStaticTestSuite stdArgs y
  pure $ all (\i -> y `accept` ordinaryTrace (fst (runSpecification' False i x))) r

genPattern :: Gen (OutputPattern 'TraceP)
genPattern = sized $ \size ->
  frequency $
    [ (1,pure Wildcard)
    , (1,Text <$> listOf1 (arbitraryPrintableChar `suchThat` (/= '_')))
    ] ++
    [ (4,liftA2 (<>) (resize 1 genPattern) (resize (size - 1) genPattern)) | size > 1
    ]

-- misc
xprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
xprop s = xit s . property

fprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
fprop s = fit s . property

-- disable a propertiy for faster testing
xPerfprop :: (HasCallStack, Testable prop) => String -> prop -> Spec
xPerfprop s _ = it s $ pendingWith "disabled for faster testing"
