{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
module Test.IOTest.Utils
  ( (...)
  , StringEmbedding(..)
  ) where

import Data.Kind
import Data.Proxy

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

-- alternative to Show that does not escape strings
class StringEmbedding (k::Bool) a where
  pack :: Proxy k -> a -> String
  unpack :: Proxy k -> String -> a

instance StringEmbedding 'True String where
  pack _ = id
  unpack _ = id

type IsNotString a = IsStringType a ~ 'False

instance (IsNotString a, Read a,Show a) => StringEmbedding 'False a where
  pack _ = show
  unpack _ = read

type family IsStringType (k :: Type) :: Bool where
  IsStringType String = 'True
  IsStringType a = 'False
