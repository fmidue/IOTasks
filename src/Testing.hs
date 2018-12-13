{-# LANGUAGE RecordWildCards #-}
module Testing where

import Types
import Core (inputGenerator)
import Trace (buildPTrace, checkAgainstProto)
import IOtt (runtt, IOtt)

import Test.QuickCheck

test :: IOtt() -> Spec -> IO ()
test solution spec = do
  res <- quickCheckWithResult
         stdArgs{chatty = False}
         (specProperty spec solution)
  case res of
    Success{..} -> Prelude.putStr output
    GaveUp{..} -> Prelude.putStr output
    NoExpectedFailure{..} -> Prelude.putStr output
    InsufficientCoverage{..} -> Prelude.putStr output
    -- TODO: imrove error reporting for disjucnctions and in general
    Failure{..} -> do
      let (l1:l2:ls) = lines output
      let r = read :: String -> [(String, Int)]
      Prelude.putStr . unlines $ (l1 : show (fst <$> r l2) : ls)

specProperty :: Spec -> IOtt () -> Property
specProperty spec solution =
  let paths = getPaths spec
      gens = inputGenerator . getCore <$> paths
      tests = testWithProto solution <$> paths
      props = zipWith forAll gens tests
  in disjoin props

testWithProto :: IOtt () -> PathSpec -> [(String,Int)] -> Property
testWithProto prog path inputs =
  let trace = runtt prog (fst <$> inputs)
      proto = buildPTrace path inputs
      (b, err) = checkAgainstProto trace proto
  in counterexample err $ property b
