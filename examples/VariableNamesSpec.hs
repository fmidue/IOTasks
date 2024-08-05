{-# LANGUAGE TypeApplications #-}
module VariableNamesSpec where
import Prelude hiding
  (putChar,putStr,putStrLn,print,getChar,getLine,readLn)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (isSuccess)

import Test.IOTasks
import Test.IOTasks.Term (oEval)
import Test.IOTasks.Var ( someVar )
import Test.IOTasks.ValueMap ( emptyValueMap, insertValue, wrapValue )

import Data.Functor ((<&>))

specification :: Specification
specification =
  readInput x ints AssumeValid <>
  writeOutput [resultOf $ liftOpaque (f,"f") $ currentValue x]
  where
    x = intVar "myVariable"
    f :: Integer -> Integer
    f = (+1)

program :: MonadTeletype m => m ()
program = do
  x <- readLn
  print $ x+1

varnameGen :: Gen String
varnameGen = sized $ \sz -> do
  n <- chooseInt (1,sz)
  x <- elements $ ['a'..'z'] ++ ['A'..'Z']
  xs <- vectorOf (n-1) (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_','\''])
  pure (x:xs)

spec :: Spec
spec =
  context "variable names and opaque terms" $ do
    describe "varnameGen" $
      prop "should only generate legal names" $ forAll varnameGen $ \str ->
        currentValue @Integer (intVar str) == currentValue (intVar str)

    describe "terms with random variable names" $ do
      prop "should all evaluate to a value (simple)" $ forAll varnameGen $ \str ->
        let var = intVar str
        in (\(_,v) -> v == 1) $ oEval @Integer
            (insertValue (wrapValue (0 :: Integer)) (someVar var) $ emptyValueMap [someVar var])
            (liftOpaque ((+1),"+1") $ currentValue var)
      prop "should all evaluate to a value (merge-current)" $ forAll (liftA2 (,) varnameGen varnameGen) $ \(str1,str2) ->
        let
          var1 = intVar str1
          var2 = intVar str2
        in (\(_,v) -> v == 2) $ oEval @Integer
            (insertValue (wrapValue (1 :: Integer)) (someVar var2) $ insertValue (wrapValue (0 :: Integer)) (someVar var1) $ emptyValueMap [someVar var1,someVar var2])
            (liftOpaque ((+1),"+1") $ currentValue [var1, var2])
      prop "should all evaluate to a value (merge-all)" $ forAll (liftA2 (,) varnameGen varnameGen) $ \(str1,str2) ->
        let
          var1 = intVar str1
          var2 = intVar str2
        in (\(_,v) -> v == 3) $ oEval @Integer
            (insertValue (wrapValue (1 :: Integer)) (someVar var2) $ insertValue (wrapValue (0 :: Integer)) (someVar var1) $ emptyValueMap [someVar var1,someVar var2])
            (liftOpaque ((+1),"+1") $ length' $ as @[Integer] $ allValues [var1, var2])
      prop "order in VarExpr should not matter" $ forAll (liftA2 (,) varnameGen varnameGen) $ \(str1,str2) ->
        let
          var1 = intVar str1
          var2 = intVar str2
          evalExample (x,y) = oEval @Integer
            (insertValue (wrapValue (1 :: Integer)) (someVar x) $ insertValue (wrapValue (0 :: Integer)) (someVar y) $ emptyValueMap [someVar x,someVar y])
            (liftOpaque ((+1),"+1") $ currentValue [x, y])
        in evalExample (var1,var2) == evalExample (var2,var1)
      it "satisfies specification" $
        (taskCheckOutcome program specification <&> isSuccess) `shouldReturn` True
