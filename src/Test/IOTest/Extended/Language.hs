module Test.IOTest.Extended.Language
  ( Specification, branch, tillE, nop, e
  , NumType(..), VarName, Term, S.AbstractPattern(Exactly, Contains,Everything), S.optional

  , readInputS, writeOutputS, writeOutputP
  , readInputI, writeOutputI

  , getCurrentS, getAllS
  , getCurrentI, getAllI
  ) where

import qualified Test.IOTest.Internal.Specification as S
import Test.IOTest.Internal.Specification (VarName, NumType)
import qualified Test.IOTest.Internal.Term as T

type Specification = S.Specification VarName String
type Term = T.Term VarName String

getCurrentS :: VarName -> Term String
getCurrentS = T.getCurrent

getAllS :: VarName -> Term [String]
getAllS = T.getAll

getCurrentI :: VarName -> Term Int
getCurrentI x = read <$> getCurrentS x

getAllI :: VarName -> Term [Int]
getAllI x = fmap read <$> getAllS x

readInputS :: VarName -> Specification
readInputS x = S.ReadInput x Nothing

readInputI :: VarName -> NumType -> Specification
readInputI x ty = S.ReadInput x (Just ty)

writeOutputS :: [Term String] -> Specification
writeOutputS = S.WriteOutput

writeOutputI :: [Term Int] -> Specification
writeOutputI ts = S.WriteOutput $ fmap show <$> ts

writeOutputP :: [S.AbstractPattern VarName String String] -> Specification
writeOutputP = S.WriteOutputP

branch :: Term Bool -> Specification -> Specification -> Specification
branch = S.Branch

tillE :: Specification -> Specification
tillE = S.TillE

nop :: Specification
nop = S.Nop

e :: Specification
e = S.E
