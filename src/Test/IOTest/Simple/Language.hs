module Test.IOTest.Simple.Language
  ( Specification, readInput, writeOutput, branch, tillE, nop, e
  , NumType(..), VarName, Term, S.optional
  , getCurrent, getAll
  ) where

import qualified Test.IOTest.Internal.Specification as S
import Test.IOTest.Internal.Specification (NumType, VarName)
import qualified Test.IOTest.Internal.Term as T

type Specification = S.Specification VarName Int
type Term = T.Term VarName Int

getCurrent :: VarName -> Term Int
getCurrent = T.getCurrent

getAll :: VarName -> Term [Int]
getAll = T.getAll

readInput :: VarName -> NumType -> Specification
readInput = S.ReadInput

writeOutput :: [Term Int] -> Specification
writeOutput = S.WriteOutput

branch :: Term Bool -> Specification -> Specification -> Specification
branch = S.Branch

tillE :: Specification -> Specification
tillE = S.TillE

nop :: Specification
nop = S.Nop

e :: Specification
e = S.E
