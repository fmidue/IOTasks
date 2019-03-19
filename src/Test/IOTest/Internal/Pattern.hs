{-# LANGUAGE DeriveFunctor #-}
module Test.IOTest.Internal.Pattern
  ( AbstractPattern(..)
  , ConcretePattern(..)
  , check
  , description
  ) where

import Test.IOTest.Internal.Term
import Test.IOTest.Utils
import Data.List

data AbstractPattern v s a
  = Exactly (Term v s a) String -- 1. term to "equal" the string 2. description of the term
  | Contains (Term v s a) String -- 1. term to be contained in the string 2. description of the term
  | Everything
  | NoOutput

data ConcretePattern a
  = ExactlyC a String -- 1. term to "equal" the string 2. description of the term
  | ContainsC a String -- 1. term to be contained in the string 2. description of the term
  | EverythingC
  deriving (Eq,Ord,Functor)

instance Show a => Show (ConcretePattern a) where
  show = description

check :: StringRep a => ConcretePattern a -> String -> Bool
check (ExactlyC t _) s = to t == s
check (ContainsC t _) s = to t `isInfixOf` s
check EverythingC _ = True

description :: ConcretePattern a -> String
description (ExactlyC _ d) = d
description (ContainsC _ d) = "a string containing " ++ d
description EverythingC = "some string"
