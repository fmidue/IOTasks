{-# LANGUAGE DataKinds #-}
import Test.Hspec

import OutputPattern

import Testing
import qualified Testing.Random as Random
import Example
import Interpreter

import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success, stdArgs)
import Control.Applicative (liftA2)
import Control.Monad.Loops (allM)

main :: IO ()
main = hspec $ do
  context "solver based testing" $ do
    describe "prog1" $ do
      it "taskCheck example1 specification" $
        taskCheck prog1 example1 `shouldReturn` Success
      it "does not fulfill example2 specification" $
        taskCheck prog1 example2 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        taskCheck prog1 example3 `shouldNotReturn` Success

    describe "prog2" $ do
      it "taskCheck example2 specification" $
        taskCheck prog2 example2 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        taskCheck prog2 example1 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        taskCheck prog2 example3 `shouldNotReturn` Success

    describe "prog3" $ do
      it "taskCheck example3 specification" $
        taskCheck prog3 example3 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        taskCheck prog3 example1 `shouldNotReturn` Success
      it "does not fulfill example2 specification" $
        taskCheck prog3 example2 `shouldNotReturn` Success

    context "static test generation" $ do
      describe "prog2" $ do
        it "fulfills tests generated from example2 specification" $
          ((\is -> taskCheckOn is prog2 example2) <$> generateStaticTestSuite stdArgs example2) `shouldReturn` Success
        it "does not fulfill tests generated from example1 specification" $
          ((\is -> taskCheckOn is prog2 example1) <$> generateStaticTestSuite stdArgs example1) `shouldNotReturn` Success
        it "does not fulfill tests generated from example3 specification" $
          ((\is -> taskCheckOn is prog2 example3) <$> generateStaticTestSuite stdArgs example3) `shouldNotReturn` Success

    context "interpretation" $
      it "all interpretations of a specification satisfy that specification" $
        allM (\p -> (== Success) <$> taskCheck p example1) (interpret example1) `shouldReturn` True

    context "string/newline semantics" $ do
      describe "stringS1" $ do
        it "is satisfied by stringP1" $
          taskCheck stringP1 stringS1 `shouldReturn` Success
        it "is satisfied by stringP2" $
          taskCheck stringP2 stringS1 `shouldReturn` Success
        it "is satisfied by stringP3" $
          taskCheck stringP3 stringS1 `shouldReturn` Success

      describe "stringS2" $ do
        it "is satisfied by stringP1" $
          taskCheck stringP1 stringS2 `shouldReturn` Success
        it "is not satisfied by stringP2" $
          taskCheck stringP2 stringS2 `shouldNotReturn` Success
        it "is satisfied by stringP3" $
          taskCheck stringP3 stringS2 `shouldReturn` Success

  context "random testing" $ do
    describe "prog1" $ do
      it "taskCheck example1 specification" $
        Random.taskCheck prog1 example1 `shouldReturn` Success
      it "does not fulfill example2 specification" $
        Random.taskCheck prog1 example2 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        Random.taskCheck prog1 example3 `shouldNotReturn` Success

    describe "prog2" $ do
      it "taskCheck example2 specification" $
        Random.taskCheck prog2 example2 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        Random.taskCheck prog2 example1 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        Random.taskCheck prog2 example3 `shouldNotReturn` Success

    describe "prog3" $ do
      it "taskCheck example3 specification" $
        Random.taskCheck prog3 example3 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        Random.taskCheck prog3 example1 `shouldNotReturn` Success
      it "does not fulfill example2 specification" $
        Random.taskCheck prog3 example2 `shouldNotReturn` Success

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

genPattern :: Gen (OutputPattern 'TraceP)
genPattern = sized $ \size ->
  frequency $
    [ (1,pure Wildcard)
    , (1,Text <$> listOf1 (arbitraryPrintableChar `suchThat` (/= '_')))
    ] ++
    [ (4,liftA2 (<>) (resize 1 genPattern) (resize (size - 1) genPattern)) | size > 1
    ]
