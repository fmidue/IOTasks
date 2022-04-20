import Test.Hspec

import Testing
import qualified Testing.Random as Random
import Example

main :: IO ()
main = hspec $ do
  context "solver based testing" $ do
    describe "prog1" $ do
      it "fulfills example1 specification" $
        fulfills defaultConfig prog1 example1 `shouldReturn` Success
      it "does not fulfill example2 specification" $
        fulfills defaultConfig prog1 example2 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        fulfills defaultConfig prog1 example3 `shouldNotReturn` Success

    describe "prog2" $ do
      it "fulfills example2 specification" $
        fulfills defaultConfig prog2 example2 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        fulfills defaultConfig prog2 example1 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        fulfills defaultConfig prog2 example3 `shouldNotReturn` Success

    describe "prog3" $ do
      it "fulfills example3 specification" $
        fulfills defaultConfig prog3 example3 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        fulfills defaultConfig prog3 example1 `shouldNotReturn` Success
      it "does not fulfill example2 specification" $
        fulfills defaultConfig prog3 example2 `shouldNotReturn` Success

    context "static test generation" $ do
      describe "prog2" $ do
        it "fulfills tests generated from example2 specification" $
          ((\is -> fulfillsOn is prog2 example2) <$> generateStaticTestSuite defaultConfig example2) `shouldReturn` Success
        it "does not fulfill tests generated from example1 specification" $
          ((\is -> fulfillsOn is prog2 example1) <$> generateStaticTestSuite defaultConfig example1) `shouldNotReturn` Success
        it "does not fulfill tests generated from example3 specification" $
          ((\is -> fulfillsOn is prog2 example3) <$> generateStaticTestSuite defaultConfig example3) `shouldNotReturn` Success

  context "random testing" $ do
    describe "prog1" $ do
      it "fulfills example1 specification" $
        Random.fulfills Random.defaultConfig prog1 example1 `shouldReturn` Success
      it "does not fulfill example2 specification" $
        Random.fulfills Random.defaultConfig prog1 example2 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        Random.fulfills Random.defaultConfig prog1 example3 `shouldNotReturn` Success

    describe "prog2" $ do
      it "fulfills example2 specification" $
        Random.fulfills Random.defaultConfig prog2 example2 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        Random.fulfills Random.defaultConfig prog2 example1 `shouldNotReturn` Success
      it "does not fulfill example3 specification" $
        Random.fulfills Random.defaultConfig prog2 example3 `shouldNotReturn` Success

    describe "prog3" $ do
      it "fulfills example3 specification" $
        Random.fulfills Random.defaultConfig prog3 example3 `shouldReturn` Success
      it "does not fulfill example1 specification" $
        Random.fulfills Random.defaultConfig prog3 example1 `shouldNotReturn` Success
      it "does not fulfill example2 specification" $
        Random.fulfills Random.defaultConfig prog3 example2 `shouldNotReturn` Success
