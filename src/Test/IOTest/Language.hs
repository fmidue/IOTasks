{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.IOTest.Language
  ( Specification, readInput, writeOutput, branch, tillExit, nop, exit
  , writeFixedOutput
  , ValueSet(..), Varname, Term, optional
  , getCurrent, getAll
  , getCurrentS, getAllS
  , getCurrentGeneric, getAllGeneric
  , LinearPattern, buildPattern
  , intValues
  , stringTerms, nonStringTerms
  ) where

import Test.IOTest.Utils
import Test.IOTest.Specification
import qualified Test.IOTest.Term as T
import Test.IOTest.Term (Term)
import Test.IOTest.Environment (Varname)
import Test.IOTest.ValueSet
import Test.IOTest.Pattern

import Data.Proxy
import Data.Typeable

readInput :: Varname -> ValueSet -> Specification
readInput x vs = Spec [ReadInput x vs]

type HasStringTerms (s::Bool) = Proxy s

stringTerms :: HasStringTerms 'True
stringTerms = Proxy
nonStringTerms :: HasStringTerms 'False
nonStringTerms = Proxy

writeOutput :: StringEmbedding a => [LinearPattern] -> [Term a] -> Specification
writeOutput ps ts = Spec [WriteOutput False ps ts]

writeFixedOutput :: [LinearPattern] -> Specification
writeFixedOutput ps = Spec [WriteOutput False ps ([] :: [Term String])]

branch :: Term Bool -> Specification -> Specification -> Specification
branch t s1 s2 = Spec [Branch t s1 s2]

tillExit :: Specification -> Specification
tillExit s = Spec [TillE s]

nop :: Specification
nop = mempty

exit :: Specification
exit = Spec [E]

intValues :: [Int] -> ValueSet
intValues = ValueSet

getCurrent :: (Typeable a, StringEmbedding a) => Varname -> Term a
getCurrent = T.getCurrent

getCurrentS :: Varname -> Term String
getCurrentS = T.getCurrent

getAll :: (Typeable a, StringEmbedding a) => Varname -> Term [a]
getAll = T.getAll

getAllS :: Varname -> Term [String]
getAllS = T.getAll

getCurrentGeneric :: (Typeable a, StringEmbedding a) => Varname -> Term a
getCurrentGeneric = T.getCurrent

getAllGeneric :: (Typeable a, StringEmbedding a) => Varname -> Term [a]
getAllGeneric = T.getAll
