{-# LANGUAGE DefaultSignatures #-}
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

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

-- alternative to Show/Read that does not escape strings
class StringEmbedding a where
  pack :: a -> String
  default pack :: Show a => a -> String
  pack = show
  unpack :: String -> a
  default unpack :: Read a => String -> a
  unpack = read

instance StringEmbedding String where
  pack = id
  unpack = id

instance StringEmbedding Bool
instance StringEmbedding Int
