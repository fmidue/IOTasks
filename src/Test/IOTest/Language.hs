{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.IOTest.Language
  ( Specification, readInput, writeOutput, branch, tillEnd, nop, end
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
import Test.IOTest.Internal.Specification
import qualified Test.IOTest.Internal.Term as T
import Test.IOTest.Internal.Term (Term)
import Test.IOTest.Internal.Context (Varname)
import Test.IOTest.Internal.ValueSet
import Test.IOTest.Internal.Pattern

import Data.Proxy
import Data.Typeable

readInput :: Varname -> ValueSet -> Specification
readInput x vs = Spec [ReadInput x vs]

type HasStringTerms (s::Bool) = Proxy s

stringTerms :: HasStringTerms 'True
stringTerms = Proxy
nonStringTerms :: HasStringTerms 'False
nonStringTerms = Proxy

writeOutput :: StringEmbedding s a => [LinearPattern] -> [Term a] -> HasStringTerms s -> Specification
writeOutput ps ts proxy = Spec [WriteOutput proxy False ps ts]

writeFixedOutput :: [LinearPattern] -> Specification
writeFixedOutput ps = Spec [WriteOutput (Proxy @'True) False ps ([] :: [Term String])]

branch :: Term Bool -> Specification -> Specification -> Specification
branch t s1 s2 = Spec [Branch t s1 s2]

tillEnd :: Specification -> Specification
tillEnd s = Spec [TillE s]

nop :: Specification
nop = mempty

end :: Specification
end = Spec [E]

intValues :: [Int] -> ValueSet
intValues = ValueSet (Proxy @'False)

getCurrent :: (Typeable a, StringEmbedding 'False a) => Varname -> Term a
getCurrent = T.getCurrent (Proxy @'False)

getCurrentS :: Varname -> Term String
getCurrentS = T.getCurrent (Proxy @'True)

getAll :: (Typeable a, StringEmbedding 'False a) => Varname -> Term [a]
getAll = T.getAll (Proxy @'False)

getAllS :: Varname -> Term [String]
getAllS = T.getAll (Proxy @'True)

getCurrentGeneric :: forall s a . (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Term a
getCurrentGeneric = T.getCurrent

getAllGeneric :: forall s a . (Typeable a, StringEmbedding s a) => Proxy s -> Varname -> Term [a]
getAllGeneric = T.getAll
