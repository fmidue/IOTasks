module BranchingStrictnessSpec where

import Test.IOTasks
import Test.Hspec
import Test.IOTasks.Trace (ordinaryTrace)

specificaiton :: Specification
specificaiton =
  readInput x ints AssumeValid <>
  branch (currentValue x .==. intLit 1)
    nop
    (readInput x undefined undefined) -- cannot be completely undefined as we do one complete travesal for the initial variable environment
  where
    x = intVar "x"

spec :: Spec
spec =
  describe "branch" $ it "runSpecification does not evaluate unneeded branches" $
    let
      (t1,_) = runSpecification specificaiton ["1"]
      (t2,_) = runSpecification specificaiton ["1","2"]
    in ordinaryTrace t1 == ordinaryTrace t2
