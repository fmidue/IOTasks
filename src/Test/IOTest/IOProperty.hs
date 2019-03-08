{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IOTest.IOProperty (
  IOProperty,
  fulfills,
  fulfillsNot,
  --generalize2,
  --generalize3
) where

import Test.IOTest.IOtt (IOtt, runProgram)
import Test.IOTest.Language
import Test.IOTest.Trace
import Test.IOTest.TraceSet

import Data.Coerce

import Control.Arrow ((&&&))
import Control.Monad.Trans.Writer.Lazy
import Test.QuickCheck
import Data.Bifunctor (bimap)

import qualified Data.Set as S

data IOProperty prog spec = Fulfills prog spec | FulfillsNot prog spec

fulfills :: a -> b -> IOProperty a b
fulfills = Fulfills
fulfillsNot :: a -> b -> IOProperty a b
fulfillsNot = FulfillsNot

instance Testable (IOProperty (IOtt ()) (Specification VarName)) where
  property (prog `Fulfills` spec) = specProperty spec prog
  property (prog `FulfillsNot` spec) = expectFailure $ specProperty spec prog

instance (Show b, Arbitrary b, Testable (IOProperty a' b'), Coercible b a) => Testable (IOProperty (a -> a') (b -> b')) where
  property (Fulfills f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `Fulfills` g x)
  property (FulfillsNot f g) = forAllShrink arbitrary shrink (\x -> f (coerce x) `FulfillsNot` g x)

-- | Properties for programs and specifications with matching argument types.
--   The 'Coercible' constraint allows for implict generator selection for example via types from "Test.QuickCheck.Modifiers"
-- generalize2 :: Coercible a b =>
--                   (IOtt () -> Specification VarName -> IOProperty)
--                   -> (a -> IOtt ())
--                   -> (b -> Specification VarName)
--                   -> b -> IOProperty
-- generalize2 f g h x = f (g (coerce x)) (h x)
--
-- generalize3 :: (Coercible a c, Coercible b d) =>
--                   (IOtt () -> Specification VarName -> IOProperty)
--                   -> (a -> b -> IOtt ())
--                   -> (c -> d -> Specification VarName)
--                   -> c -> d -> IOProperty
-- generalize3 f g h x y = f (g (coerce x) (coerce y)) (h x y)

specProperty :: Specification VarName -> IOtt () -> Property
specProperty spec program =
  let gen = traceGen spec
      prop t = testTrace ((id &&& inputs) t) program
  in forAll gen prop

testTrace :: (NTrace Int, [Int]) -> IOtt () -> Property
testTrace (tg,i) p =
  let t = normalize $ runProgram (show <$> i) p
      w = t `isCoveredBy` bimap (S.map (fmap show)) show tg
      (result,msg) = runWriter w
  in counterexample (msg ++ "\n  program trace: " ++ show (bimap (S.map (fmap Wrap)) Wrap t)) result

newtype StringWrapper = Wrap String deriving (Eq,Ord)
instance Show StringWrapper where
  show (Wrap s) = s
