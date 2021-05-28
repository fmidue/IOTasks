{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTasks.Examples.PTermExamples where

import Test.IOTasks hiding (SpecTerm)

import Data.Term.PTerm

type SpecTerm = PTerm Varname

-- Example 1:
-- read natural number n, then read n integers and sum them
ex1P :: Specification SpecTerm
ex1P =
  readInput "n" (intValues [0..100]) <>
  tillExit (
    branch ( Length (getAll @Int "xs") :== getCurrent "n")
     ( readInput "xs" (intValues [-100..100]) )
     exit
  ) <>
  writeOutput [var 0] [Sum $ getAll @Int "xs"]

ex1P' :: Specification SpecTerm
ex1P' =
  readInput "n" (intValues [0..100]) <>
  tillExit (
    branch ( Length (getAll @Int "xs") :> getCurrent "n")
     ( readInput "xs" (intValues [-100..100]) <>
       readInput "xs" (intValues [-100..100]) )
     exit
  ) <>
  writeOutput [var 0] [Sum $ getAll @Int "xs"]

-- With possible extra outputs
ex1PPattern :: Specification SpecTerm
ex1PPattern =
  writeFixedOutput [anything] <>
  readInput "n" nats <>
  tillExit (
    branch (Length (getAll @Int "xs") :== getCurrent "n")
     ( optional (writeOutput [anything <> var 0 <> anything] [Length (getAll @Int "xs")]) <>
       readInput "xs" ints
     )
     exit
  ) <>
  writeOutput [anything <> var 0 <> anything] [Sum $ getAll @Int "xs"]

-- read till last two numbers sum to 0 than count positive numbers divisible by 3
-- (setting QuickCheck's maxSize parameter reduces the time to check this significantly (e.g. 30 seems OK))
ex2P :: Specification SpecTerm
ex2P =
  repeatSpec 2 (readInput "xs" ints) <> --otherwise the condition will throw an exception
  readUntil "xs" (let xs = getAll "xs" in Length xs :> Lit 1 :&& (Last xs :+ Last (Init xs) :== Lit (0 :: Int)) ) ints <>
  writeOutput [anything <> var 0 <> anything] [count $ getAll @Int "xs"]
  where count xs = Length $ Filter (\x -> x > 0 && (x `mod` 3 == 0)) xs

-- Example 3:
-- read till zero then sum
ex3P :: Specification SpecTerm
ex3P =
  tillExit $
    readInput "x" ints <>
    when (Lit 0 :== getCurrent @Int "x")
      (writeOutput [anything <> var 0 <> anything] [Sum $ getAll @Int "x"] <> exit)
